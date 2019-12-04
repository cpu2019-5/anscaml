package net.akouryy.anscaml
package arch.tig

class GraphDrawer {
  private[this] val res = new StringBuilder

  //noinspection SpellCheckingInspection
  def apply(p: asm.Program): String = {
    res.clear()
    res ++=
    """digraph Program {
      |graph [fontname = "Monaco", fontsize = 13, ranksep = 0.5];
      |node [shape = box, fontname = "Monaco", fontsize = 12];
      |edge [fontname = "Monaco", fontsize = 12];
      |""".stripMargin

    for (f <- p.functions) {
      res ++=
      s"""subgraph cluster_${Math.abs(f.hashCode)} {
         |label="${f.name.name}";
         |color=green;
         |""".stripMargin

      for (j <- f.body.jumps.valuesIterator) {
        res ++= (j match {
          case asm.StartFun(i, ob) =>
            s"""$i[label = "StartFun.${i.index}"; shape = box3d];
               |$i -> $ob;
               |""".stripMargin
          case asm.Return(i, v, ib) =>
            s"""$i[label = "Return.${i.index}"; shape = box3d];
               |$ib -> $i [label="$v"];
               |""".stripMargin
          case asm.Condition(i, op, l, r, ib, tob, fob) =>
            s"""$i[
               |  label = "$i\n$l $op $r";
               |  shape = box; style = rounded;
               |];
               |$ib -> $i;
               |$i -> $tob [label=true];
               |$i -> $fob [label=false];
               |""".stripMargin
          case asm.Merge(i, inputs, v, ob) =>
            val inputEdges =
              inputs.map(ib => s"""${ib._2} -> $i [label="${ib._1}"];""").mkString
            s"""$i[label = "Merge.${i.index}"; shape = box; style = rounded];
               |$inputEdges
               |$i -> $ob [label="$v"];
               |""".stripMargin
        })
      }

      for (asm.Block(i, lines, _, _) <- f.body.blocks.valuesIterator) {
        if (lines.isEmpty) {
          res ++= s"""$i [label = "$i\\l(0行)"]"""
        } else {
          val ls = lines.take(15).map(l => s"${l.dest} = ${l.inst}").mkString("\\l")
          val msg = if (lines.sizeIs <= 15) "" else s"略\\l"
          res ++= s"""$i [label = "$i (${lines.size}行)\\l-----\\l$ls\\l$msg"]"""
        }
      }

      res ++= s"}\n"
    }

    res ++= "}\n"
    res.toString
  }
}
