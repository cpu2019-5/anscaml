package net.akouryy.anscaml.algo

import scala.util.chaining._

class UnionFind(n: Int) {
  private[this] val parents = Array.tabulate(n)(identity)
  private[this] val ranks = Array.fill(n)(0)

  /** representative element (代表元) */
  def repr(i: Int): Int = {
    val p = parents(i)
    if (p == i) i
    else repr(p).tap(parents(i) = _)
  }

  def same(i: Int, j: Int): Boolean = repr(i) == repr(j)

  def merge(i: Int, j: Int): Boolean = {
    val iRoot = repr(i)
    val jRoot = repr(j)
    if (iRoot == jRoot) {
      false
    } else {
      val iRank = ranks(iRoot)
      val jRank = ranks(jRoot)
      val (parent, child) = if (iRank > jRank) (iRoot, jRoot) else (jRoot, iRoot)
      parents(parent) = child
      if (iRank == jRank) ranks(parent) += 1
      true
    }
  }
}
