package net.akouryy.anscaml
package arch.tig

import asm.{BlockIndex, C, JumpIndex, Line, Ty, V, XID, XReg, XVar}
import base._
import knorm.KNorm
import knorm.KNorm.{KCProgram, KClosed}
import swarm.SwarmIndex
import syntax.BinOp
import typ.Typ

import scala.collection.{immutable, mutable}

object Specializer {

  private sealed trait GConst

  private final case class GCInt(i: Int) extends GConst

  private final case class GCFloat(f: Float) extends GConst

  private final case class GCArrayImm(addr: Int, len: Int, elem: ID) extends GConst

  private final case class GCOther(addr: Int, closed: KClosed) extends GConst

}

class Specializer {

  import Specializer._

  private[this] val gConsts = mutable.Map[XID, (Ty, GConst)]()
  private[this] var gConstsListRev = List[ID]()
  private[this] var gConstsSumSize: Int = _

  private[this] val tyEnv = mutable.Map[XVar, Ty]()
  private[this] val fnTypEnv = mutable.Map[ID, Typ]()

  private[this] var currentChart: asm.Chart = _
  private[this] var currentBlockIndex: asm.BlockIndex = _
  private[this] var currentInputJumpIndex: asm.JumpIndex = _
  private[this] var currentLines = mutable.ListBuffer[asm.Line]()
  private[this] var currentFunIsLeaf: Boolean = _
  private[this] var currentForUpdater = Option.empty[KNorm.LoopUpdater]

  private[this] val mviEnv = mutable.Map[XVar, asm.Mvi]()

  private[this] var cx: TigContext = _
  private[this] var kSwarmIndices: Map[ID, SwarmIndex] = _

  private[this] def loadGConstsInfo(gcs: List[(Entry, KClosed)]): Unit = {
    gConsts.clear()
    gConstsListRev = Nil
    gConstsSumSize = 0

    for ((entry, cl) <- gcs) {
      val (gc, size) = cl.raw match {
        case KNorm.KInt(i) => (GCInt(i), 0)
        case KNorm.KFloat(f) => (GCFloat(f), 0)
        case KNorm.KArray(len, elem) =>
          gConsts.get(XVar(len.str)) match {
            case Some((_, GCInt(len))) => (GCArrayImm(gConstsSumSize, len, elem), len)
            case _ => (GCOther(gConstsSumSize, cl), 1) // 即値かポインタなのでサイズ1
          }
        case _ => (GCOther(gConstsSumSize, cl), 1)
      }
      gConsts(XVar(entry.name.str)) = (Ty(entry.typ), gc)
      gConstsListRev ::= entry.name
      gConstsSumSize += size
    }
  }

  private[this] def wrapVar(v: ID): XVar = {
    val vv = XVar(v.str)
    gConsts.get(vv) match {
      case None => vv
      case Some((ty, gc)) =>
        val line = gc match {
          case GCInt(i) => asm.Mvi.int(i)
          case GCFloat(f) => asm.Mvi.float(f)
          case GCArrayImm(addr, _, _) => asm.Mvi.int(addr)
          case GCOther(addr, _) => asm.Load(XReg.ZERO, C(Word(addr)), ty match {
            case asm.TyTuple(_) => asm.MITuple
            case _ => asm.MIUnknown
          })
        }
        val x = XVar.generate(vv.idStr + ID.Special.GC_INSTANCE, allowEmptySuffix = true)
        cx.swarmIndices.updateByGet(x, vv)
        tyEnv(x) = ty
        currentLines += Line(CM(s"[SP] use gConst ${v.str}"), x, line)
        x
    }
  }

  private[this] def specializeInitialization(): Unit = {
    currentLines ++= Seq(
      Line(CM(s"[SP] base of heap = (gConst size)"), XReg.HEAP, asm.Mvi.int(gConstsSumSize)),
      // ここでスタックポインタを初期化するとその前にmainのスタック拡張が来てしまうのでEmitterでやる
    )

    gConstsListRev.reverseIterator.foreach { gConst =>
      gConsts(XVar(gConst.str)) match {
        case (_, _: GCInt | _: GCFloat) | (asm.TyArray(asm.TyUnit), _: GCArrayImm) => // no store
        case (_, GCArrayImm(addr, len, elem)) =>
          val e = wrapVar(elem)
          for (i <- 0 until len) {
            currentLines += Line(CM(s"[SP] def gConst ${gConst.str}"), XReg.DUMMY,
              asm.Store(XReg.ZERO, C.int(addr + i), e, asm.MIArray(kSwarmIndices(gConst), i)))
          }
        case (ty, GCOther(addr, kcl)) =>
          val gcVal = XVar.generate(gConst.str + ID.Special.GC_VAL, allowEmptySuffix = true)
          cx.swarmIndices.updateByGet(gcVal, XVar(gConst.str))
          specializeExpr(gcVal, isTail = false, kcl)
          currentLines += Line(CM(s"[SP] def gConst ${gConst.str}"),
            XReg.DUMMY, asm.Store(XReg.ZERO, C.int(addr), gcVal, ty match {
              case asm.TyTuple(_) => asm.MITuple
              case _ => asm.MIUnknown
            }))
      }
    }
  }

  /**
    * @return 標準関数が存在するならそれを表す非空の命令列、存在しないならNil
    */
  def specializeInlineStdlib(cm: Comment, dest: XID, fn: String, args: List[XID]): List[Line] =
    (fn, args) match {
      case ("$ext_print_char", List(x)) =>
        List(Line(cm :+ "[SP] $ext_print_char", dest, asm.Write(x)))
      case ("$ext_read_char", List()) =>
        List(Line(cm :+ "[SP] $ext_read_char", dest, asm.Read))
      case ("$ext_fneg", List(x)) =>
        List(Line(cm :+ "[SP] $ext_fneg", dest, asm.BinOpVTree(asm.FnegCond, x, XReg.C_MINUS_ONE)))
      case ("$ext_fabs", List(x)) =>
        List(Line(cm :+ "[SP] $ext_fabs", dest, asm.BinOpVTree(asm.FnegCond, x, x)))
      case ("$ext_floor", List(x)) =>
        List(Line(cm :+ "[SP] $ext_floor", dest, asm.UnOpTree(asm.Floor, x)))
      case ("$ext_float_of_int", List(x)) =>
        List(Line(cm :+ "[SP] $ext_float_of_int", dest, asm.UnOpTree(asm.Itof, x)))
      case ("$ext_int_of_float", List(x)) =>
        List(Line(cm :+ "[SP] $ext_int_of_float", dest, asm.UnOpTree(asm.Ftoi, x)))
      case ("$ext_sqrt", List(x)) =>
        List(Line(cm :+ "[SP] $ext_sqrt", dest, asm.UnOpTree(asm.FSqrt, x)))
      case ("$ext_bits_of_float", List(x)) =>
        List(Line(cm :+ "[SP] $ext_bits_of_float", dest, asm.Mv(x)))
      case ("$ext_float_of_bits", List(x)) =>
        List(Line(cm :+ "[SP] $ext_float_of_bits", dest, asm.Mv(x)))
      case _ => Nil
    }

  private[this] def specializeExpr(dest: XID, isTail: Boolean, cl: KClosed): Unit = {
    val cm = cl.comment
    cl.raw match {
      case KNorm.KInt(i) =>
        val mvi = asm.Mvi.int(i)
        dest.asXVar.foreach(mviEnv(_) = mvi)
        currentLines += Line(cm, dest, mvi)
        ()
      case KNorm.KFloat(f) =>
        val mvi = asm.Mvi.float(f)
        dest.asXVar.foreach(mviEnv(_) = mvi)
        currentLines += Line(cm, dest, mvi)
        ()
      case KNorm.BinOpTree(op, left, right) =>
        val l = wrapVar(left)
        val r = wrapVar(right)
        currentLines += Line(cm, dest, op match {
          case BinOp.Add => asm.BinOpVCTree(asm.Add, l, asm.V(r))
          case BinOp.Sub => asm.BinOpVTree(asm.Sub, l, r)
          case BinOp.Shl => asm.BinOpVCTree(asm.Sha, l, asm.V(r))
          case BinOp.Shr =>
            val neg = XVar.generate(s"${r.idStr}$$neg")
            assert(!cx.swarmIndices.contains(r))
            currentLines += Line(CM(s"[SP]Negate for Shr"),
              neg, asm.BinOpVTree(asm.Sub, XReg.ZERO, r))
            asm.BinOpVCTree(asm.Sha, l, asm.V(neg))
          case BinOp.Band => asm.BinOpVCTree(asm.Band, l, asm.V(r))
          case BinOp.Div => // TODO: remove
            asm.BinOpVTree(asm.Div, l, r)
          case BinOp.Mul | BinOp.Mod => ????(cl)
          case BinOp.Fadd => asm.BinOpVTree(asm.Fadd, l, r)
          case BinOp.Fsub => asm.BinOpVTree(asm.Fsub, l, r)
          case BinOp.Fmul => asm.BinOpVTree(asm.Fmul, l, r)
          case BinOp.Fdiv => asm.BinOpVTree(asm.Fdiv, l, r)
        })
        ()
      case KNorm.Var(v) =>
        val x = wrapVar(v)
        currentLines += Line(cm, dest, tyEnv(x) match {
          case asm.TyUnit => asm.Nop
          case _ => asm.Mv(x)
        })
        ()
      case KNorm.KTuple(Nil) => assert(dest == XReg.DUMMY)
      case KNorm.KTuple(elems) =>
        var i = 0
        for (elem <- elems) {
          val e = wrapVar(elem)
          if (tyEnv(e) != asm.TyUnit) {
            currentLines += Line(NC, XReg.DUMMY,
              asm.Store(XReg.HEAP, C(Word(i)), e, asm.MITuple))
            // (cx.swarmIndices(dest.asXVar.get), i)
            i += 1
          }
        }
        currentLines ++= Seq(
          Line(cm, dest, asm.Mv(XReg.HEAP)),
          Line(NC, XReg.HEAP, asm.BinOpVCTree(asm.Add, XReg.HEAP, asm.C(Word(i)))),
        )
        ()
      case raw: KNorm.LoopUpdater =>
        assert(currentForUpdater.isEmpty)
        currentForUpdater = Some(raw)
      case KNorm.KArray(len, elem) =>
        val l = wrapVar(len)
        val e = wrapVar(elem)
        currentLines += Line(cm, dest, asm.NewArray(V(l), e))
        ()
      case KNorm.Get(array, index) =>
        val a = wrapVar(array)
        val i = wrapVar(index)
        if (tyEnv(a) != asm.TyArray(asm.TyUnit)) {
          val _ = currentLines += Line(cm, dest, asm.Load(a, V(i),
            asm.MIArray(kSwarmIndices(array), mviEnv.get(i).map(_.value.int))))
        }
      case KNorm.Put(array, index, value) =>
        assert(dest == XReg.DUMMY)
        val a = wrapVar(array)
        val i = wrapVar(index)
        val v = wrapVar(value)
        if (tyEnv(a) != asm.TyArray(asm.TyUnit)) {
          val addr = XVar.generate(array.str + ID.Special.SPECIALIZE_ADDR)
          // TODO: swarm for addr
          currentLines += Line(NC, addr, asm.BinOpVCTree(asm.Add, a, V(i)))
          currentLines += Line(cm, XReg.DUMMY, asm.Store(addr, C.int(0), v,
            asm.MIArray(kSwarmIndices(array), mviEnv.get(i).map(_.value.int))))
          ()
        }
      case KNorm.ApplyDirect(fn, args) =>
        val Typ.TFun(_, retTyp) =
          if (fn.startsWith(ID.Special.EXTERNAL_PREFIX))
            typ.Constrainer.ExtEnv(ID(fn.substring(5)))
          else
            fnTypEnv(ID(fn))
        if (retTyp == Typ.TUnit) {
          assert(dest == XReg.DUMMY, dest)
        }
        val as = args map wrapVar
        specializeInlineStdlib(cm, dest, fn, as) match {
          case Nil =>
            currentFunIsLeaf &&= isTail
            currentLines += Line(cm, dest, asm.CallDir(fn, as, None)) // no stdlib
          case lines => currentLines ++= lines
        }
        ()
      case KNorm.ApplyClosure(_, _) => ???
      case KNorm.CLet(Entry(_, typ.Typ.TUnit), bound, kont) =>
        specializeExpr(XReg.DUMMY, isTail = false, bound)
        specializeExpr(dest, isTail, kont)
      case KNorm.CLet(entry, bound, kont) =>
        val v = XVar(entry.name.str)
        specializeExpr(v, isTail = false, bound)
        tyEnv(v) = Ty(entry.typ)
        specializeExpr(dest, isTail, kont)
      case KNorm.CLetTuple(elems, bound, kont) =>
        val b = wrapVar(bound)
        var i = 0
        for (elem <- elems) {
          val v = XVar(elem.name.str)
          if (elem.typ != Typ.TUnit) {
            tyEnv(v) = Ty(elem.typ)
            currentLines += Line(cm, v, asm.Load(b, C(Word(i)), asm.MITuple))
            // (kSwarmIndices(bound), i)
            i += 1
          } else {
            tyEnv(v) = asm.TyUnit
          }
        }
        specializeExpr(dest, isTail, kont)
      case KNorm.CLetClosure(_, _, _, _) => ???

      case KNorm.CIfCmp(op, left, right, tru, fls) =>
        val l = wrapVar(left)
        val r = wrapVar(right)
        val condJumpIndex = JumpIndex.generate()
        val branchingBlockIndex = currentBlockIndex
        // 各分岐先は1つ以上のブロックから構成されるが、その先頭ブロック
        val trueStartBlockIndex = BlockIndex.generate()
        val falseStartBlockIndex = BlockIndex.generate()
        val trueDest, falseDest =
          if (dest == XReg.DUMMY) {
            dest
          } else {
            val d = XVar.generate(ID.generate().str)
            dest.asXVar.foreach(cx.swarmIndices.updateByGet(d, _))
            d
          }

        // ifの前のブロックを登録
        currentChart.blocks(branchingBlockIndex) =
          asm.Block(branchingBlockIndex, currentLines.toList, currentInputJumpIndex, condJumpIndex)

        // if分岐を登録
        currentChart.jumps(condJumpIndex) = asm.Branch(
          cm,
          condJumpIndex,
          asm.CmpOp.fromSyntax(op).fold(asm.Branch.CondV(_, l, r), asm.Branch.CondVC(_, l, V(r))),
          currentBlockIndex, trueStartBlockIndex, falseStartBlockIndex,
        )

        // 真分岐
        currentBlockIndex = trueStartBlockIndex
        currentInputJumpIndex = condJumpIndex
        currentLines.clear()
        specializeExpr(trueDest, isTail, tru)
        // 真分岐の最後のブロックを登録
        val trueLastBlockIndex = currentBlockIndex
        val trueLastBlockLines = currentLines.toList
        val trueLastBlockInputIndex = currentInputJumpIndex

        // 偽分岐
        currentBlockIndex = falseStartBlockIndex
        currentInputJumpIndex = condJumpIndex
        currentLines.clear()
        specializeExpr(falseDest, isTail, fls)
        // 偽分岐の最後のブロックを登録
        val falseLastBlockIndex = currentBlockIndex

        // 双方の分岐先のspecializeが終わった後に後続のIndex生成
        val mergeJumpIndex = JumpIndex.generate()
        val kontBlockIndex = BlockIndex.generate()

        currentChart.blocks(trueLastBlockIndex) =
          asm.Block(trueLastBlockIndex, trueLastBlockLines, trueLastBlockInputIndex, mergeJumpIndex)
        currentChart.blocks(falseLastBlockIndex) =
          asm.Block(falseLastBlockIndex, currentLines.toList, currentInputJumpIndex, mergeJumpIndex)

        // マージブロックを登録
        currentChart.jumps(mergeJumpIndex) =
          asm.Merge(
            NC,
            mergeJumpIndex,
            List(
              asm.MergeInput(trueLastBlockIndex, trueDest),
              asm.MergeInput(falseLastBlockIndex, falseDest),
            ),
            dest, kontBlockIndex,
          )

        // 継続
        currentBlockIndex = kontBlockIndex
        currentInputJumpIndex = mergeJumpIndex
        currentLines.clear()

      case KNorm.CForCmp(op, left, right, negated, loopVars, initVars, body, kont) =>
        /*
          1. ForLoopTop
          2. body top
          3. ...body...
          4. body bottom
          5. ForLoopBottom
          6. kont
         */
        val l = wrapVar(left)
        val r = wrapVar(right)
        val loopXVars = loopVars.map(id => XVar(id.str))
        val initXVars = initVars.map(wrapVar)
        for ((iv, lv) <- initVars.zipStrict(loopVars)) {
          tyEnv(XVar(lv.str)) = tyEnv(XVar(iv.str))
        }
        // 1. ForLoopTop
        val inputBlockIndex = currentBlockIndex
        val inputBlockInputJumpIndex = currentInputJumpIndex
        val inputBlockLines = currentLines.toList
        val forTopJumpIndex = JumpIndex.generate()
        // 2. body top
        val bodyTopBlockIndex = BlockIndex.generate()
        currentBlockIndex = bodyTopBlockIndex
        currentInputJumpIndex = forTopJumpIndex
        currentLines.clear()
        assert(currentForUpdater.isEmpty)
        // 3. ...body...
        specializeExpr(XReg.DUMMY, isTail = false, body)
        // 4. body bottom
        val bodyBottomBlockIndex = currentBlockIndex
        val bodyBottomBlockInputIndex = currentInputJumpIndex
        val updXVars = currentForUpdater match {
          case Some(KNorm.LoopUpdater(elems)) =>
            currentForUpdater = None
            elems.map(wrapVar)
          case None => !!!!(body)
        }
        val bodyBottomBlockLines = currentLines.toList // get after `wrapVar`s
        // 5. ForLoopBottom
        val forBottomJumpIndex = JumpIndex.generate()
        // 6. kont
        val kontTopBlockIndex = BlockIndex.generate()
        currentBlockIndex = kontTopBlockIndex
        currentInputJumpIndex = forTopJumpIndex
        currentLines.clear()
        specializeExpr(dest, isTail, kont)

        // register all
        val merges = loopXVars.zipStrict(initXVars).zipStrict(updXVars).map {
          case ((l, i), u) => asm.ForLoopVar(in = i, upd = u, loop = l)
        }
        currentChart.blocks(inputBlockIndex) = asm.Block(
          inputBlockIndex, inputBlockLines, inputBlockInputJumpIndex, forTopJumpIndex,
        )
        currentChart.jumps(forTopJumpIndex) = asm.ForLoopTop(
          cm, forTopJumpIndex,
          asm.CmpOp.fromSyntax(op).fold(asm.Branch.CondV(_, l, r), asm.Branch.CondVC(_, l, V(r))),
          negated, merges,
          inputBlockIndex, forBottomJumpIndex, bodyTopBlockIndex, kontTopBlockIndex,
        )
        currentChart.blocks(bodyBottomBlockIndex) = asm.Block(
          bodyBottomBlockIndex, bodyBottomBlockLines, bodyBottomBlockInputIndex, forBottomJumpIndex,
        )
        currentChart.jumps(forBottomJumpIndex) = asm.ForLoopBottom(
          NC, forBottomJumpIndex, bodyBottomBlockIndex, forTopJumpIndex,
        )

      case KNorm.CGeneralLoop(loopVars, initVars, body) =>
        /*
          1. GeneralLoopTop
          2. body top
          3. ...body...
          4. body bottom
          5. GeneralLoopBottomNext
          6. GeneralLoopBottomEnd
          7. kont
         */
        loopVars
    }
  }

  private[this] def specializeFDef(cFDef: KNorm.CFDef, gcsOpt: Option[List[(Entry, KClosed)]])
  : asm.FDef = {
    mviEnv.clear()

    tyEnv ++= cFDef.args.map(e => XVar(e.name.str) -> Ty(e.typ))
    val fnTyp = asm.Fn.fromTyp(cFDef.entry.typ)

    currentChart = new asm.Chart
    currentBlockIndex = asm.BlockIndex.generate()
    currentFunIsLeaf = true

    // 最初のStartFunジャンプを登録
    val startFunJumpIndex = JumpIndex.generate()
    currentChart.jumps(startFunJumpIndex) =
      asm.StartFun(NC, startFunJumpIndex, currentBlockIndex)
    currentInputJumpIndex = startFunJumpIndex
    currentLines.clear()

    for (gcs <- gcsOpt) { // main関数限定
      loadGConstsInfo(gcs)
      specializeInitialization()
    }

    val retVar =
      if (fnTyp.ret == asm.TyUnit) {
        XReg.DUMMY
      } else {
        XVar.generate(s"${cFDef.entry.name.str}$$ret")
      }
    specializeExpr(retVar, isTail = true, cFDef.body)

    if (gcsOpt.isDefined) { // mainの最後にexit擬似関数の呼び出しを加える
      currentLines += Line(NC, XReg.DUMMY, asm.CallDir(ID.Special.ASM_EXIT_FUN, Nil, None))
    }

    // 最後のブロックとその後のReturnジャンプを登録
    val returnJumpIndex = JumpIndex.generate()
    currentChart.blocks(currentBlockIndex) =
      asm.Block(currentBlockIndex, currentLines.toList, currentInputJumpIndex, returnJumpIndex)
    currentChart.jumps(returnJumpIndex) =
      asm.Return(NC, returnJumpIndex, retVar, None, currentBlockIndex)

    asm.FDef(
      cFDef.entry.name.str,
      cFDef.args.map(a => XVar(a.name.str)),
      currentChart,
      fnTyp,
      asm.FDefInfo(isLeaf = currentFunIsLeaf, safeRegs = immutable.SortedSet[XReg]()),
    )
  }

  def apply(cl: KCProgram, sw: Map[ID, swarm.SwarmIndex]): (asm.Program, TigContext) = {
    kSwarmIndices = sw
    /*util.Using.resource(new java.io.PrintWriter("../temp/sg-2.txt")) {
      base.PPrinter.writeTo(_, sw)
    }*/
    cx = new TigContext
    cx.swarmIndices ++= sw.map { case k -> v => XVar(k.str) -> v }

    fnTypEnv.clear()

    fnTypEnv ++= cl.fDefs.map(_.entry.toPair)

    val main = specializeFDef(
      KNorm.CFDef(Entry(ID(ID.Special.MAIN), Typ.TFun(Nil, Typ.TUnit)), Nil, Nil, cl.main),
      Some(cl.gConsts)
    )

    val fDefs = cl.fDefs.map(specializeFDef(_, None))

    val p = asm.Program(gConstsSumSize, tyEnv.toMap, main :: fDefs)

    (p, cx)
  }
}
