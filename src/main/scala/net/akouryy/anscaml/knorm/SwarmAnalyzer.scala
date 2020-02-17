package net.akouryy.anscaml
package knorm

import algo.UnionFind
import base.{Entry, ID}
import knorm.KNorm._
import swarm.{SRArray, SRTuple, SwarmIndex, SwarmRelation}
import typ.Typ

import scala.collection.mutable

class SwarmAnalyzer {
  def apply(norm: KNorm): Map[ID, SwarmIndex] = {
    Logger.log("SwA", "Start")
    typEnv.clear()
    paramsEnv.clear()
    swarmIndicesRaw.clear()
    fnResEnv.clear()
    tupleResEnv.clear()
    buildEnv(norm)
    swarmChildrenRaw = Array.fill(swarmIndicesRaw.size)(mutable.Map())
    uf = new UnionFind(swarmIndicesRaw.size)
    mergeAll(norm)

    /*util.Using.resource(new java.io.PrintWriter("../temp/sg-1.txt")) { pw =>
      base.PPrinter.writeTo(pw, swarmIndicesRaw.keys.groupBy(swarmIndex))
      base.PPrinter.writeTo(pw,
        swarmChildrenRaw.zipWithIndex.flatMap { case (v, k) =>
          Option.when(k == uf.repr(k) && v.nonEmpty)(k -> v.map { case (r, i) => r -> uf.repr(i) })
        }.toMap)
    }*/

    swarmIndicesRaw.map { case (id, i) => id -> SwarmIndex(uf.repr(i)) }.toMap
  }

  private[this] val typEnv = mutable.Map[ID, Typ]()
  private[this] val paramsEnv = mutable.Map[ID, List[Entry]]()
  private[this] val fnResEnv = mutable.Map[ID, ID]()
  private[this] val forResEnv = mutable.Map[ForCmp, ID]()
  private[this] val tupleResEnv = mutable.Map[KTuple, ID]()
  private[this] val swarmIndicesRaw = mutable.Map[ID, Int]()
  private[this] var swarmChildrenRaw: Array[mutable.Map[SwarmRelation, Int]] = _
  private[this] var uf: UnionFind = _

  private[this] def swarmIndex(v: ID) = uf.repr(swarmIndicesRaw(v))

  private[this] def isSwarmingTyp(typ: Typ) = typ match {
    case Typ.TArray(_) | Typ.TTuple(_ :: _) => true // TODO: 長さ0の配列は群れない
    case _ => false
  }

  private[this] def doesSwarm(v: ID) = isSwarmingTyp(typEnv(v))

  private[this] def createSingletonSwarm(v: ID) =
    if (doesSwarm(v)) swarmIndicesRaw(v) = swarmIndicesRaw.size

  private[this] def mergeSwarm(a: Int, b: Int): Unit = {
    assert(a == uf.repr(a))
    assert(b == uf.repr(b))
    if (uf.merge(a, b)) {
      val strong = uf.repr(a)
      val weak = a + b - strong
      val strongChildrenRaw = swarmChildrenRaw(strong)
      val weakChildrenRaw = swarmChildrenRaw(weak)
      for {
        (rel, cRaw) <- strongChildrenRaw
        dRaw <- weakChildrenRaw.get(rel)
      } mergeSwarm(uf.repr(cRaw), uf.repr(dRaw))
      for {
        (rel, dRaw) <- weakChildrenRaw
        if !strongChildrenRaw.contains(rel)
      } strongChildrenRaw(rel) = uf.repr(dRaw)
    }
  }

  private[this] def relate(a: ID, bAndRel: (ID, Option[SwarmRelation])): Unit = {
    val (b, bRel) = bAndRel
    if (a.str.startsWith("dummy") || b.str.startsWith("dummy")) return // TODO: 長さ0の配列は群れない
    val ai = swarmIndex(a)
    val bi = swarmIndex(b)
    val bChildrenRaw = swarmChildrenRaw(bi)
    bRel match {
      case Some(bRel) => bChildrenRaw.get(bRel) match {
        case Some(bc) => mergeSwarm(ai, uf.repr(bc))
        case None => bChildrenRaw(bRel) = ai
      }
      case None => mergeSwarm(ai, bi)
    }
  }

  private[this] def buildEnv(norm: KNorm): Unit = {
    norm.raw match {
      case IfCmp(_, _, _, tru, fls) => buildEnv(tru); buildEnv(fls)
      case raw @ ForCmp(_, _, _, _, loopVars, initVars, body, kont) =>
        // left and right does not swarm
        val updater = ID.generate(s"$$upd")
        val initTyps = initVars.map(typEnv)
        typEnv(updater) = Typ.TTuple(initTyps)
        typEnv ++= loopVars.zipStrict(initTyps).toMap
        forResEnv(raw) = updater
        createSingletonSwarm(updater)
        loopVars.foreach(createSingletonSwarm)
        buildEnv(body)
        buildEnv(kont)
      case Let(entry, bound, kont) =>
        typEnv += entry.toPair
        createSingletonSwarm(entry.name)
        buildEnv(bound)
        buildEnv(kont)
      case LetTuple(elems, _: ID, kont) =>
        typEnv ++= elems.map(_.toPair)
        elems.foreach(v => createSingletonSwarm(v.name))
        buildEnv(kont)
      case LetRec(FDef(entry, params, body, _), kont) =>
        val res = ID.generate(s"${entry.name.str}$$ret")
        typEnv ++= entry.toPair :: (res -> entry.typ.asInstanceOf[Typ.TFun].ret) ::
                   params.map(_.toPair)
        paramsEnv(entry.name) = params
        fnResEnv(entry.name) = res
        createSingletonSwarm(entry.name)
        createSingletonSwarm(res)
        params.foreach(p => createSingletonSwarm(p.name))
        buildEnv(body)
        buildEnv(kont)
      case kt @ KTuple(elems) =>
        val res = ID.generate("$tuple")
        typEnv(res) = Typ.TTuple(elems.map(typEnv))
        tupleResEnv(kt) = res
        createSingletonSwarm(res)
      case LoopUpdater(elems) =>
        val res = ID.generate("$tuple")
        typEnv(res) = Typ.TTuple(elems.map(typEnv))
        tupleResEnv(KTuple(elems)) = res
        createSingletonSwarm(res)
      case _ =>
    }
  }

  private[this] def mergeAll(norm: KNorm): List[(ID, Option[SwarmRelation])] = {
    norm.raw match {
      case IfCmp(_, _, _, tru, fls) =>
        mergeAll(tru) ++ mergeAll(fls)
      case raw @ ForCmp(_, _, _, _, loopVars, initVars, body, kont) =>
        val updater = forResEnv(raw)
        for (((lv, iv), i) <- loopVars.zipStrict(initVars).zipWithIndex if doesSwarm(lv)) {
          relate(lv, (iv, None))
          relate(lv, (updater, Some(SRTuple(i))))
        }
        mergeAll(body)
        mergeAll(kont)
      case Let(entry, bound, kont) =>
        val bs = mergeAll(bound)
        if (doesSwarm(entry.name)) {
          bs.foreach(relate(entry.name, _))
        }
        mergeAll(kont)
      case LetTuple(elems, bound, kont) =>
        for ((elem, i) <- elems.zipWithIndex if doesSwarm(elem.name)) {
          relate(elem.name, bound -> Some(SRTuple(i)))
          //          println(s"${elem.name.str} ~ ${bound.str}<$i>")
        }
        mergeAll(kont)
      case LetRec(FDef(entry, _, body, _), kont) =>
        val b = mergeAll(body)
        val ret = fnResEnv(entry.name)
        if (doesSwarm(ret)) {
          b.foreach(relate(ret, _))
        }
        mergeAll(kont)
      case Var(v) if doesSwarm(v) =>
        List(v -> None)
      //            println(s"${entry.name.str} ~ ${v.str}")
      case Get(a, _) if isSwarmingTyp(typEnv(a).asInstanceOf[Typ.TArray].elem) =>
        List(a -> Some(SRArray))
      //            println(s"${entry.name.str} ~ ${a.str}[]")
      case Put(a, _, v) if doesSwarm(v) =>
        relate(v, a -> Some(SRArray))
        Nil
      //            println(s"${a.str}[] ~ ${v.str}")
      case kt @ KTuple(elems) =>
        val res = tupleResEnv(kt)
        for ((elem, i) <- elems.zipWithIndex if doesSwarm(elem)) {
          relate(elem, res -> Some(SRTuple(i)))
          //              println(s"${entry.name.str}<$i> ~ ${elem.str}")
        }
        Option.when(doesSwarm(res))(res -> None).toList
      case LoopUpdater(elems) =>
        val res = tupleResEnv(KTuple(elems))
        for ((elem, i) <- elems.zipWithIndex if doesSwarm(elem)) {
          relate(elem, res -> Some(SRTuple(i)))
        }
        Option.when(doesSwarm(res))(res -> None).toList
      case Apply(fn, args, _) =>
        for ((param, arg) <- paramsEnv(fn).zipStrict(args) if doesSwarm(arg)) {
          relate(param.name, arg -> None)
          //              println(s"${param.name.str} ~ ${arg.str}")
        }
        if (isSwarmingTyp(typEnv(fn).asInstanceOf[Typ.TFun].ret))
          List(fnResEnv(fn) -> None)
        else
          Nil
      case _ => Nil
    }
  }
}
