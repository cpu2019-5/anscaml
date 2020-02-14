package net.akouryy.anscaml
package arch.tig

import asm.{XID, XReg, XVar}
import base._
import Coalescer._

import scala.collection.{immutable, mutable}
import scala.util.control.Breaks

/**
  * @see Sebastian Hack and Gerhard Goos. 2008. Copy coalescing by graph recoloring.
  *      In Proceedings of the 29th ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI ’08).
  *      Association for Computing Machinery, New York, NY, USA, 227–237. DOI:https://doi.org/10.1145/1375581.1375610
  */
class Coalescer(
  rawIntf: RegisterAllocator#IGraph, rawAffs: Affinities, rawColors: Map[XID, XReg],
  availableRegs: immutable.SortedSet[XReg],
) {
  private[this] val ALPHA = 0.1
  private[this] val MAX_DEQUEUE_ITER = 100
  private[this] val MAX_RECOLOR_NODE_COUNT = 20

  private[this] val queue = mutable.PriorityQueue[Chunk]()
  private[this] val tempColors = mutable.Map[XID, XReg]()

  private[this] val colors: mutable.Map[XID, XReg] = {
    val m = rawColors.to(mutable.Map)
    m --= XReg.NORMAL_REGS_SET -- availableRegs
    for (xr <- availableRegs) m(xr) = xr
    m
  }

  private[this] val affinities: Affinities =
    (for {
      x <- colors.keysIterator
    } yield {
      (x, rawAffs.getOrElse(x, Map()).filter(colors contains _._1))
    }).toMap

  private[this] val isFixated: mutable.Map[XID, Boolean] = {
    val m = mutable.Map[XID, Boolean]()
    for (Util.The(xv: XVar) <- colors.keysIterator) m(xv) = false
    for (xr <- availableRegs) m(xr) = true
    m
  }

  private[this] val intf: Interferences = {
    val m = mutable.Map[XID, Set[XID]]()
    for (Util.The(xv: XVar) <- colors.keysIterator) {
      m(xv) = rawIntf.get(xv).foldF(_.filter(colors.contains), Set())
    }
    for (xr <- availableRegs) m(xr) = Set()
    for {
      (xv, is) <- rawIntf
      Util.The(xr: XReg) <- is
      if availableRegs contains xr
    } {
      m(xr) += xv
    }
    m.toMap
  }

  private[this] val adm: Map[XID, Set[XReg]] = {
    val m = mutable.Map[XID, Set[XReg]]()
    for (Util.The(xv: XVar) <- colors.keysIterator) {
      m(xv) = availableRegs -- intf(xv).flatMap(_.asXReg)
    }
    for (xr <- availableRegs) {
      m(xr) = Set(xr)
    }
    m.toMap
  }

  /**
    * [5.0] main
    */
  def coalesce: Map[XID, XReg] = {
    queue.clear()
    queue ++= createInitialChunks().filter(_.elems.sizeIs >= 2)
    var cnt = 0
    while (cnt < MAX_DEQUEUE_ITER && queue.nonEmpty) {
      val chunk = queue.dequeue()
      queue ++= recolorChunk(chunk).filter(_.elems.sizeIs >= 2)
      cnt += 1
    }
    colors.toMap
  }

  /** [5.1] createInitialChunks(queue) */
  private[this] def createInitialChunks() = {
    val chunks = colors.keys.map(x => new Chunk(Set(x))).to(mutable.Set)
    val flatAfs = for ((v, afs) <- affinities.toSeq; (w, gain) <- afs) yield (v, w, gain)

    for ((v, w, _) <- flatAfs.sortBy { case (_, _, gain) => -gain }) {
      val vc = chunks.find(_.elems contains v).get
      val wc = chunks.find(_.elems contains w).get
      if ((vc ne wc) && !(vc interferesWith wc)) {
        chunks --= Seq(vc, wc)
        chunks += new Chunk(vc.elems ++ wc.elems)
      }
    }
    chunks
  }

  /** [5.2] p(v, c) */
  private[this] def nodeColorPreference(x: XID, color: XReg) = {
    val a = adm(x)
    if (a contains color) {
      val k = availableRegs.size
      (1.0 + k - a.size) / k
    } else {
      0.0
    }
  }

  /** [5.2] recolorChunk */
  private[this] def recolorChunk(chunk: Chunk): Seq[Chunk] = {
    var best = Option.empty[(XReg, Chunk)]
    val chunkSize = chunk.elems.size
    Breaks.breakable {
      for (color <- chunk.computeColorOrder) {
        var nSuccessful = 0
        for (node <- chunk.elems) {
          if (changeTo(node, color)) {
            nSuccessful += 1
            isFixated(node) = true
          }
        }
        for (node <- chunk.elems) {
          isFixated(node) = false
        }
        if (nSuccessful > 1) {
          val localBest = fragmentChunk(chunk, color)
          if (best.forall(_._2.totalGain < localBest.totalGain)) {
            best = Some((color, localBest))
          }
          if (nSuccessful == chunkSize) {
            Breaks.break()
          }
        }
      }
    }
    best match {
      case Some((bestColor, bestChunk)) =>
        for (node <- bestChunk.elems) {
          changeTo(node, bestColor)
          isFixated(node) = true
        }
        getNewChunks(chunk, bestChunk)
      case None =>
        Nil
    }
  }

  private[this] def splitIntoAffComponents(rawElems: Set[XID]) = {
    val elems = rawElems.to(mutable.Set)
    val queue = mutable.Queue[XID]()

    for (root <- elems.toSeq; if elems contains root) yield {
      val currentElems = mutable.Set[XID]()
      queue.clear()
      queue.enqueue(root)
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        if (elems contains node) {
          elems -= node
          currentElems += node
          queue.enqueueAll(affinities(node).keys)
        }
      }
      new Chunk(currentElems.toSet)
    }
  }

  /**
    * [5.2] fragmentChunk
    */
  private[this] def fragmentChunk(base: Chunk, color: XReg) = {
    splitIntoAffComponents(base.elems.filter(colors(_) == color)).max
  }

  /**
    * [5.2] getNewChunks
    */
  private[this] def getNewChunks(base: Chunk, toRemove: Chunk) = {
    splitIntoAffComponents(base.elems -- toRemove.elems)
  }

  /** [5.3] commit */
  private[this] def commit(nodeList: mutable.Stack[XID]) = {
    for (node <- nodeList) {
      colors(node) = tempColors(node)
      tempColors -= node
    }

    nodeList.clear()
  }

  /**
    * [5.3] rollback
    *
    * @param gauge nodeList中に残す要素数
    */
  private[this] def rollback(nodeList: mutable.Stack[XID], gauge: Int) = {
    while (nodeList.size > gauge) {
      tempColors -= nodeList.pop()
    }
  }

  /** [5.3] isLoose */
  private[this] def isLoose(node: XID) = !isFixated(node) && !tempColors.contains(node)

  /** [5.3] getColor */
  private[this] def getColor(node: XID) = tempColors.getOrElse(node, colors(node))

  /**
    * [5.3] makeAvoidList
    */
  private[this] def makeAvoidList(node: XID, avoidColor: XReg): Seq[XReg] = {
    val prio = mutable.Map[XReg, Double]()
    val neighCol = mutable.Map[XReg, Int]()
    for (xr <- availableRegs) {
      prio(xr) = nodeColorPreference(node, xr)
      neighCol(xr) = 0
    }
    for (neigh <- intf(node)) {
      if (isLoose(neigh)) {
        neighCol(colors(neigh)) += 1
      } else {
        prio(colors(neigh)) = 0.0
      }
    }
    val nLoose = intf(node).count(isLoose)
    if (nLoose > 0) {
      prio.mapValuesInPlace((c, p) => p * (1.0 - neighCol(c).toDouble / nLoose))
    }
    prio(avoidColor) = 0.0

    availableRegs.toSeq.filter(prio(_) > 0.0).sortBy(prio)(Ordering.Double.IeeeOrdering).reverse
  }

  private[this] var recolorNodeCount = 0

  /** [5.3] recolorNode */
  private[this] def recolorNode(
    node: XID, colorList: Seq[XReg], changed: mutable.Stack[XID]
  ): Boolean =
    recolorNodeCount < MAX_RECOLOR_NODE_COUNT && {
      recolorNodeCount += 1

      val gauge = changed.size

      colorList.exists { color =>
        tempColors(node) = color
        changed.push(node)
        val success = intf(node).forall { neigh =>
          getColor(neigh) != color ||
          isLoose(neigh) && recolorNode(neigh, makeAvoidList(neigh, color), changed)
        }
        if (!success) {
          rollback(changed, gauge)
        }
        success
      }
    }

  /** [5.3] changeTo */
  private[this] def changeTo(node: XID, targetColor: XReg): Boolean = {
    assert(tempColors.isEmpty)
    if (colors(node) == targetColor) {
      true
    } else if (isLoose(node) && adm(node).contains(targetColor)) {
      val changed = mutable.Stack[XID]()
      recolorNodeCount = 0
      if (recolorNode(node, List(targetColor), changed)) {
        commit(changed)
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  private[this] var chunkCnt = 0

  private[this] final class Chunk(val elems: Set[XID]) extends Ordered[Chunk] {
    private val chunkUID = chunkCnt
    chunkCnt += 1

    def interferesWith(that: Chunk): Boolean = {
      elems.exists(e => (intf(e) & that.elems).nonEmpty)
    }

    /**
      * [5.2] p(C, c)
      */
    private val colorPreference: Map[XReg, Double] =
      availableRegs.toSet[XReg].map(c => (c, elems.map(nodeColorPreference(_, c)).sum)).toMap

    /**
      * [5.2] d(C, c)
      */
    private[this] def colorDislike(color: XReg): Double = {
      val X = queue.toSeq.filter(this.interferesWith)
      1.0 - X.map(_.colorPreference(color)).sum / X.size
    }

    /**
      * [5.2] computeColorOrder
      */
    def computeColorOrder: IndexedSeq[XReg] = {
      availableRegs.toIndexedSeq.sortBy(
        c => (1 - ALPHA) * colorPreference(c) + ALPHA * colorDislike(c)
      )(Ordering.Double.IeeeOrdering).reverse
    }

    /** [5.1] cost(C) */
    lazy val totalGain: Int = {
      (for {
        v <- elems
        (w, gain) <- affinities(v)
        if elems contains w
      } yield gain)
        .sum
    }

    /** `(this compare that) == 0` only if `this == that` */
    override def compare(that: Chunk): Int = {
      Ordering.Tuple2[Int, Int].compare((totalGain, chunkUID), (that.totalGain, that.chunkUID))
    }
  }

}

object Coalescer {

  type Interferences = Map[XID, Set[XID]]
  type Affinities = Map[XID, Map[XID, Int]]

}
