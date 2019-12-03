package net.akouryy.anscaml
package arch.tig

class GraphDrawer {
  private[this] val res = new StringBuilder

  //noinspection SpellCheckingInspection
  def apply(p: asm.Program): String = {
    res.clear()
    res ++=
    """digraph Program {
      |graph [fontname = "Monaco", fontsize="10"];
      |node [shape = box, fontname = "Monaco", fontsize="10"];
      |edge [fontname = "Monaco", fontsize="10"];
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
          res ++= s"""$i [label = "$i\nempty"]"""
        } else if (lines.sizeIs <= 20) {
          val ls = lines.map(l => s"${l.dest} = ${l.inst}").mkString("\n")
          res ++= s"""$i [label = "$i\n-----\n$ls"]"""
        }
        else {
          res ++= s"""$i [label = "$i\n(略)"]"""
        }
      }

      res ++= s"}\n"
    }

    res ++= "}\n"
    res.toString
  }
}
