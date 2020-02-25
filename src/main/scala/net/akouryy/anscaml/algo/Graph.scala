package net.akouryy.anscaml.algo

object Graph {
  def apply(nVertices: Int, flatEdges: Seq[(Int, Int)]): Graph = {
    val destss = Array.fill(nVertices)(Set[Int]())
    for ((i, j) <- flatEdges) {
      destss(i) += j
    }
    Graph(destss)
  }

  final case class SCCQuotient(toSCCIndex: Int => Int, fromSCCIndex: Int => List[Int], graph: Graph)

}

final case class Graph(destss: Array[Set[Int]]) {
  val nv: Int = destss.length

  import Graph._

  def apply(i: Int): Set[Int] = destss(i)

  def reversed: Graph = {
    val revDestss = Array.fill(nv)(Set[Int]())
    for (
      (dests, from) <- destss.zipWithIndex;
      dest <- dests
    ) {
      revDestss(dest) += from
    }
    Graph(revDestss)
  }

  def sccQuotient: SCCQuotient = {
    val postOrder = {
      var po = List[Int]()
      val vis = Array.fill(nv)(false)

      def dfs(i: Int): Unit = {
        if (!vis(i)) {
          vis(i) = true
          po ::= i
          destss(i).foreach(dfs)
        }
      }

      (0 until nv).foreach(dfs)
      po
    }

    val toSCCIndex = Array.fill(nv)(-1)
    val nSCC = {
      var nSCC = 0

      def dfs(i: Int): Unit = {
        if (toSCCIndex(i) == -1) {
          toSCCIndex(i) = nSCC
          destss(i).foreach(dfs)
        }
      }

      for (i <- postOrder) {
        if (toSCCIndex(i) == -1) {
          dfs(i)
          nSCC += 1
        }
      }
      nSCC
    }

    val fromSCCIndex = Array.fill(nSCC)(List[Int]())
    for ((a, i) <- toSCCIndex.zipWithIndex) {
      fromSCCIndex(a) ::= i
    }

    val sccDestss = fromSCCIndex.zipWithIndex.map { case (js, i) =>
      js.flatMap(destss(_).map(toSCCIndex)).toSet - i
    }

    SCCQuotient(toSCCIndex, fromSCCIndex, Graph(sccDestss))
  }
}
