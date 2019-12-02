package net.akouryy.anscaml
package arch.tig

import asm.{AID, AReg, AVar, BlockIndex, JumpIndex, Line, Ty}
import base._
import knorm.KNorm
import KNorm.{KCProgram, KClosed}
import net.akouryy.anscaml.syntax.BinOp
import net.akouryy.anscaml.typ.Typ

import scala.collection.mutable

object Specializer {

  private sealed trait GConst

  private final case class GCInt(i: Int) extends GConst

  private final case class GCFloat(f: Float) extends GConst

  private final case class GCArrayImm(addr: Int, len: Int, elem: AID) extends GConst

  private final case class GCOther(addr: Int, closed: KClosed) extends GConst

}

class Specializer {

  import Specializer._

  private[this] val gConsts = mutable.Map[AID, (Ty, GConst)]()
  private[this] var gConstsListRev = List[ID]()
  private[this] var gConstsSumSize: Int = _

  private[this] def loadGConsts(gcs: List[(Entry, KClosed)]): Unit = {
    gConsts.clear()
    gConstsListRev = Nil
    gConstsSumSize = 0

    for ((entry, cl) <- gcs) {
      val (gc, size) = cl.raw match {
        case KNorm.KInt(i) => (GCInt(i), 0)
        case KNorm.KFloat(f) => (GCFloat(f), 0)
        case KNorm.Array(len, elem) =>
          gConsts.get(AVar(len)) match {
            case Some((_, GCInt(len))) => (GCArrayImm(gConstsSumSize, len, AVar(elem)), len)
            case _ => (GCOther(gConstsSumSize, cl), 1) // 即値かポインタなのでサイズ1
          }
        case _ => (GCOther(gConstsSumSize, cl), 1)
      }
      gConsts(AVar(entry.name)) = (Ty(entry.typ), gc)
      gConstsListRev ::= entry.name
      gConstsSumSize += size
    }
  }

  private[this] val tyEnv = mutable.Map[AVar, Ty]()

  private[this] var currentChart: asm.Chart = _
  private[this] var currentBlockIndex: asm.BlockIndex = _
  private[this] var currentInputJumpIndex: asm.JumpIndex = _
  private[this] var currentLines = mutable.ListBuffer[asm.Line]()

  private[this] def wrapVar(v: ID): AVar = {
    val vv = AVar(v)
    gConsts.get(vv) match {
      case None => vv
      case Some((ty, gc)) =>
        val line = gc match {
          case GCInt(i) => asm.Mvi(i)
          case GCFloat(f) => asm.Fmvi(f)
          case GCArrayImm(addr, _, _) => asm.Mvi(addr)
          case GCOther(addr, _) => asm.Loadi(AReg.REG_ZERO, addr)
        }
        val x = AVar.generate(vv, "sp_wrap")
        tyEnv(x) = ty
        currentLines += Line(x, line)
        x
    }
  }

  /**
    * @return 標準関数が存在するならそれを表す非空の命令列、存在しないならNil
    */
  def specializeInlineStdlib(dest: AID, fn: LabelID, args: List[AID]): List[Line] =
    (fn.name, args) match {
      case ("$ext_print_char", List(x)) => List(Line(dest, asm.Write(x)))
      case ("$ext_read_char", List()) => List(Line(dest, asm.Read))
      case ("$ext_fneg", List(x)) => List(Line(dest, asm.BinOpVTree(asm.Fsub, AReg.REG_ZERO, x)))
      case ("$ext_fsqr", List(x)) => List(Line(dest, asm.BinOpVTree(asm.Fmul, x, x)))
      case ("$ext_fhalf", List(x)) =>
        val half = AVar.generate("half")
        List(
          Line(half, asm.Fmvi(0.5F)),
          Line(dest, asm.BinOpVTree(asm.Fmul, x, half)),
        )
      case ("$ext_floor", List(x)) => List(Line(dest, asm.UnOpTree(asm.Floor, x)))
      case ("$ext_float_of_int", List(x)) => List(Line(dest, asm.UnOpTree(asm.Itof, x)))
      case ("$ext_bits_of_float", List(x)) => List(Line(dest, asm.Mv(x)))
      case ("$ext_float_of_bits", List(x)) => List(Line(dest, asm.Mv(x)))
      case _ => Nil
    }

  private[this] def specializeExpr(dest: AID, cl: KClosed): Unit = {
    cl.raw match {
      case KNorm.KInt(i) => currentLines += Line(dest, asm.Mvi(i))
      case KNorm.KFloat(f) => currentLines += Line(dest, asm.Fmvi(f))
      case KNorm.BinOpTree(op, left, right) =>
        val l = wrapVar(left)
        val r = wrapVar(right)
        currentLines += Line(dest, op match {
          case BinOp.Add => asm.BinOpVCTree(asm.Add, l, asm.V(r))
          case BinOp.Sub => asm.BinOpVCTree(asm.Sub, l, asm.V(r))
          case BinOp.Shl => asm.BinOpVCTree(asm.Shla, l, asm.V(r))
          case BinOp.Shr => asm.BinOpVCTree(asm.Shra, l, asm.V(r))
          case BinOp.Land => asm.BinOpVCTree(asm.Land, l, asm.V(r))
          case BinOp.Mul | BinOp.Div | BinOp.Mod =>
            throw new RuntimeException(s"[Tig Specializer] unimplemented operator $op")
          case BinOp.Fadd => asm.BinOpVTree(asm.Fadd, l, r)
          case BinOp.Fsub => asm.BinOpVTree(asm.Fsub, l, r)
          case BinOp.Fmul => asm.BinOpVTree(asm.Fmul, l, r)
          case BinOp.Fdiv => asm.BinOpVTree(asm.Fdiv, l, r)
        })
      case KNorm.Var(v) =>
        val x = wrapVar(v)
        currentLines += Line(dest, tyEnv(x) match {
          case asm.TyUnit => asm.Nop
          case _ => asm.Mv(x)
        })
      case KNorm.KTuple(Nil) => currentLines += Line(AReg.REG_DUMMY, asm.Nop)
      case KNorm.KTuple(elems) =>
        var i = 0
        for (elem <- elems) {
          val e = wrapVar(elem)
          if (tyEnv(e) != asm.TyUnit) {
            i += 1
            currentLines += Line(AReg.REG_DUMMY, asm.Storei(AReg.REG_HEAP, i, e))
          }
        }
        currentLines ++= Seq(
          Line(dest, asm.Mv(AReg.REG_HEAP)),
          Line(AReg.REG_HEAP, asm.BinOpVCTree(asm.Add, AReg.REG_HEAP, asm.C(i))),
        )
      case KNorm.Array(len, elem) =>
        val l = wrapVar(len)
        val e = wrapVar(elem)
        currentLines += Line(dest, asm.NewArray(l, e))
      case KNorm.Get(array, index) =>
        val a = wrapVar(array)
        val i = wrapVar(index)
        if (tyEnv(a) != asm.TyArray(asm.TyUnit)) {
          currentLines += Line(dest, asm.Load(a, i))
        }
      case KNorm.Put(array, index, value) =>
        val a = wrapVar(array)
        val i = wrapVar(index)
        val v = wrapVar(value)
        if (tyEnv(a) != asm.TyArray(asm.TyUnit)) {
          currentLines += Line(AReg.REG_DUMMY, asm.Store(a, i, v))
        }
      case KNorm.ApplyDirect(fn, args) =>
        val as = args map wrapVar
        specializeInlineStdlib(dest, fn, as) match {
          case Nil => currentLines += Line(dest, asm.CallDir(fn, as)) // no stdlib
          case lines => currentLines ++= lines
        }
      case KNorm.ApplyClosure(_, _) => ???
      case KNorm.CLet(entry, bound, kont) =>
        val v = AVar(entry.name)
        specializeExpr(v, bound)
        tyEnv(v) = Ty(entry.typ)
        specializeExpr(dest, kont)
      case KNorm.CLetTuple(elems, bound, kont) =>
        val b = wrapVar(bound)
        var i = 0
        for (elem <- elems) {
          val v = AVar(elem.name)
          if (elem.typ != Typ.TUnit) {
            i += 1
            tyEnv(v) = Ty(elem.typ)
            currentLines += Line(v, asm.Loadi(b, i))
          } else {
            tyEnv(v) = asm.TyUnit
          }
        }
        specializeExpr(dest, kont)
      case KNorm.CLetClosure(_, _, _, _) => ???

      case KNorm.CIfCmp(op, left, right, tru, fls) =>
        val l = wrapVar(left)
        val r = wrapVar(right)
        val condJumpIndex = JumpIndex.generate()
        val mergeJumpIndex = JumpIndex.generate()
        val branchingBlockIndex = currentBlockIndex

        // 各分岐先は1つ以上のブロックから構成されるが、その先頭ブロック
        val trueStartBlockIndex = BlockIndex.generate()
        val falseStartBlockIndex = BlockIndex.generate()
        val kontBlockIndex = BlockIndex.generate()
        val trueDest = AVar.generate(ID.generate().name)
        val falseDest = AVar.generate(ID.generate().name)

        // ifの前のブロックを登録
        currentChart.blocks(branchingBlockIndex) =
          asm.Block(branchingBlockIndex, currentLines.toList, currentInputJumpIndex, condJumpIndex)

        // if分岐を登録
        currentChart.jumps(condJumpIndex) = asm.Condition(
          condJumpIndex, op, l, r,
          currentBlockIndex, trueStartBlockIndex, falseStartBlockIndex,
        )

        // 真分岐
        currentBlockIndex = trueStartBlockIndex
        currentInputJumpIndex = condJumpIndex
        currentLines.clear()
        specializeExpr(trueDest, tru)
        // 真分岐の最後のブロックを登録
        val trueLastBlockIndex = currentBlockIndex
        currentChart.blocks(trueLastBlockIndex) =
          asm.Block(trueLastBlockIndex, currentLines.toList, currentInputJumpIndex, mergeJumpIndex)

        // 偽分岐
        currentBlockIndex = falseStartBlockIndex
        currentInputJumpIndex = condJumpIndex
        currentLines.clear()
        specializeExpr(falseDest, tru)
        // 偽分岐の最後のブロックを登録
        val falseLastBlockIndex = currentBlockIndex
        currentChart.blocks(falseLastBlockIndex) =
          asm.Block(falseLastBlockIndex, currentLines.toList, currentInputJumpIndex, mergeJumpIndex)

        // マージブロックを登録
        currentChart.jumps(mergeJumpIndex) =
          asm.Merge(
            mergeJumpIndex,
            List(trueDest -> trueLastBlockIndex, falseDest -> falseLastBlockIndex),
            dest, kontBlockIndex,
          )

        // 継続
        currentBlockIndex = kontBlockIndex
        currentInputJumpIndex = mergeJumpIndex
        currentLines.clear()
    }
  }

  private[this] def specializeFDef(cFDef: KNorm.CFDef, handleGC: Boolean): asm.FDef = {
    tyEnv ++= cFDef.args.map(e => AVar(e.name) -> Ty(e.typ))

    currentChart = new asm.Chart
    currentBlockIndex = asm.BlockIndex.generate()

    // 最初のStartFunジャンプを登録
    val startFunJumpIndex = JumpIndex.generate()
    currentChart.jumps(startFunJumpIndex) =
      asm.StartFun(startFunJumpIndex, currentBlockIndex)

    currentInputJumpIndex = startFunJumpIndex
    currentLines.clear()
    val retVar = AVar.generate(s"${cFDef.entry.name.name}$$ret")
    specializeExpr(retVar, cFDef.body)

    // 最後のブロックとその後のReturnジャンプを登録
    val returnJumpIndex = JumpIndex.generate()
    currentChart.blocks(currentBlockIndex) =
      asm.Block(currentBlockIndex, currentLines.toList, currentInputJumpIndex, returnJumpIndex)
    currentChart.jumps(returnJumpIndex) = asm.Return(returnJumpIndex, retVar, currentBlockIndex)

    asm.FDef(
      LabelID(cFDef.entry.name.name),
      cFDef.args.map(_.name),
      currentChart,
      asm.Fn.fromTyp(cFDef.entry.typ),
    )
  }

  def apply(cl: KCProgram): asm.Program = {
    loadGConsts(cl.gConsts)

    val fDefs = cl.fDefs.map(specializeFDef(_, handleGC = false))

    val main = specializeFDef(
      KNorm.CFDef(Entry(ID("$main"), Typ.TFun(Nil, Typ.TUnit)), Nil, Nil, cl.main),
      handleGC = true
    )

    asm.Program(gConstsSumSize, tyEnv.toMap, main :: fDefs)
  }
}
