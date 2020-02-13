package net.akouryy.anscaml
package arch.tig

import asm.{XID, XReg, XVar}
import base._
import Coalescer._

import scala.collection.mutable
import scala.util.control.Breaks

/**
  * @see Sebastian Hack and Gerhard Goos. 2008. Copy coalescing by graph recoloring.
  *      In Proceedings of the 29th ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI ’08).
  *      Association for Computing Machinery, New York, NY, USA, 227–237. DOI:https://doi.org/10.1145/1375581.1375610
  */
class Coalescer(
  rawIntf: RegisterAllocator#IGraph, affinities: Affinities, rawColors: Map[XID, XReg],
) {
  private[this] val ALPHA = 0.1

  private[this] val queue = mutable.PriorityQueue[Chunk]()
  private[this] val tempColors = mutable.Map[XID, XReg]()

  private[this] val colors: mutable.Map[XID, XReg] = {
    val m = rawColors.to(mutable.Map)
    for (xr <- XReg.NORMAL_REGS) m(xr) = xr
    m
  }

  private[this] val hasFixated: mutable.Map[XID, Boolean] = {
    val m = mutable.Map[XID, Boolean]()
    for (Util.The(xv: XVar) <- colors.keysIterator) m(xv) = false
    for (xr <- XReg.NORMAL_REGS) m(xr) = true
    m
  }

  private[this] val intf: Interferences = {
    val m = rawIntf.toMap[XID, Set[XID]].to(mutable.Map)
    for (xv <- colors.keysIterator; if !m.contains(xv)) {
      m(xv) = Set()
    }
    for {
      (xv, is) <- rawIntf
      Util.The(xr: XReg) <- is
    } {
      m(xr) += xv
    }
    m.toMap
  }

  /**
    * [5.0] main
    */
  def apply(): Map[XID, XReg] = {
    queue.clear()
    createInitialChunks()
    while (queue.nonEmpty) {
      val chunk = queue.dequeue()
      queue ++= recolorChunk(chunk)
    }
    colors.toMap
  }

  private[this] def adm(x: XID): Set[XReg] = x match {
    case XVar(_) => XReg.NORMAL_REGS_SET
    case xr: XReg => Set(xr)
  }

  /** [5.1] createInitialChunks(queue) */
  private[this] def createInitialChunks(): Unit = {
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
    queue ++= chunks
  }

  /** [5.2] p(v, c) */
  private[this] def nodeColorPreference(x: XID, color: XReg) = {
    val a = adm(x)
    if (a contains color) {
      val k = XReg.NORMAL_REGS.length
      (1.0 + k - a.size) / k
    } else {
      0.0
    }
  }

  /** [5.2] recolorChunk */
  private[this] def recolorChunk(chunk: Chunk): Seq[Chunk] = {
    var best = Option.empty[(XReg, Chunk)]
    Breaks.breakable {
      for (color <- chunk.computeColorOrder) {
        var nSuccessful = 0
        for (node <- chunk.elems) {
          if (changeTo(node, color)) {
            nSuccessful += 1
            hasFixated(node) = true
          }
        }
        for (node <- chunk.elems) {
          hasFixated(node) = false
        }
        if (nSuccessful > 0) {
          val localBest = fragmentChunk(chunk, color)
          if (best.forall(_._2.totalGain < localBest.totalGain)) {
            best = Some((color, localBest))
          }
          if (chunk.elems.sizeIs == nSuccessful) {
            Breaks.break()
          }
        }
      }
    }
    best match {
      case Some((bestColor, bestChunk)) =>
        for (node <- bestChunk.elems) {
          changeTo(node, bestColor)
          hasFixated(node) = true
        }
        getNewChunks(chunk, bestChunk)
      case None =>
        Nil
    }
  }

  /**
    * [5.2] fragmentChunk
    * TODO: 連結成分を返す
    */
  private[this] def fragmentChunk(chunk: Chunk, color: XReg) = {
    new Chunk(chunk.elems.filter(colors(_) == color))
  }

  /**
    * [5.2] getNewChunks
    * TODO: 連結成分ごとに分ける
    */
  private[this] def getNewChunks(chunk: Chunk, toRemove: Chunk) = {
    val elems = chunk.elems -- toRemove.elems
    if (elems.isEmpty) {
      Seq()
    } else {
      Seq(new Chunk(elems))
    }
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
  private[this] def isLoose(node: XID) = !hasFixated(node) && !tempColors.contains(node)

  /** [5.3] getColor */
  private[this] def getColor(node: XID) = tempColors.getOrElse(node, colors(node))

  /**
    * [5.3] makeAvoidList
    * TODO: やる
    */
  private[this] def makeAvoidList(neigh: XID, color: XReg): Seq[XReg] = {
    XReg.NORMAL_REGS.filter(_ != color)
  }

  /** [5.3] recolorNode */
  private[this] def recolorNode(
    node: XID, colorList: Seq[XReg], changed: mutable.Stack[XID],
  ): Boolean = {
    val gauge = changed.size

    for (color <- colorList) {
      var success = true
      tempColors(node) = color
      changed.push(node)
      Breaks.breakable {
        for (neigh <- intf(node)) {
          if (getColor(neigh) == color) {
            success = false
            if (isLoose(neigh)) {
              success = recolorNode(neigh, makeAvoidList(neigh, color), changed)
            }
          }
          if (!success) {
            rollback(changed, gauge)
            Breaks.break
          }
        }
        if (success) {
          return true
        }
      }
    }
    false
  }

  /** [5.3] changeTo */
  private[this] def changeTo(node: XID, targetColor: XReg): Boolean = {
    if (colors(node) == targetColor) {
      true
    } else if (isLoose(node) && adm(node).contains(targetColor)) {
      val changed = mutable.Stack[XID]()
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

  private[this] final class Chunk(val elems: Set[XID]) extends Ordered[Chunk] {
    def interferesWith(that: Chunk): Boolean = {
      elems.exists(e => (intf(e) & that.elems).nonEmpty)
    }

    /**
      * [5.2] p(C, c)
      * TODO: memoize
      */
    private def colorPreference(color: XReg): Double = {
      elems.map(nodeColorPreference(_, color)).sum
    }

    /**
      * [5.2] d(C, c)
      */
    private[this] def colorDislike(color: XReg): Double = {
      val X = queue.filter(this.interferesWith)
      1.0 - X.map(_.colorPreference(color)).sum / X.size
    }

    /**
      * [5.2] computeColorOrder
      */
    def computeColorOrder: IndexedSeq[XReg] = {
      import Ordering.Double.TotalOrdering

      XReg.NORMAL_REGS.sortBy(c => (1 - ALPHA) * colorPreference(c) + ALPHA * colorDislike(c))
    }

    private lazy val sortedElems = elems.toSeq.sorted

    /** [5.1] cost(C) */
    lazy val totalGain: Int = {
      val gains =
        for {
          v <- elems
          affs <- affinities.get(v).toSeq
          (w, gain) <- affs
          if elems contains w
        } yield gain
      gains.sum / 2
    }

    /** `(this compare that) == 0` only if `this == that` */
    override def compare(that: Chunk): Int = {
      import scala.math.Ordering.Implicits.seqOrdering
      (totalGain, sortedElems).compare((that.totalGain, that.sortedElems))
    }
  }

}

object Coalescer {

  type Interferences = Map[XID, Set[XID]]
  type Affinities = Map[XID, Map[XID, Int]]

}
