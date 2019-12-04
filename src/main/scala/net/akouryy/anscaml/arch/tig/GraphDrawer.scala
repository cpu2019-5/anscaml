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
            val args = f.args.map(_.str).mkString(", ")
            s"""$i[label = "StartFun.${i.indexString}"; shape = box3d];
               |$i -> $ob [label = "($args)"];
               |""".stripMargin
          case asm.Return(i, v, ib) =>
            s"""$i[label = "Return.${i.indexString}"; shape = box3d];
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
            s"""$i[label = "Merge.${i.indexString}"; shape = box; style = rounded];
               |$inputEdges
               |$i -> $ob [label="$v"];
               |""".stripMargin
        })
      }

      for (asm.Block(i, lines, _, _) <- f.body.blocks.valuesIterator) {
        if (lines.isEmpty) {
          res ++= s"""$i [label = "$i\\l(0行)"]""" + "\n"
        } else {
          val ls = lines.take(10).map(l => s"${l.dest} = ${l.inst}").mkString("\\l")
          if (lines.sizeIs <= 10) {
            res ++= s"""$i [label = "$i (${lines.size}行)\\l-----\\l$ls\\l"]""" + "\n"
          } else {
            val lsx = lines.map(l => s"${l.dest} = ${l.inst}").mkString("\\l")
            res ++=
            s"""$i [label = "$i (${lines.size}行)\\l-----\\l$ls\\l(略)"; tooltip = "$lsx"]
               |""".stripMargin
          }
        }
      }

      res ++= s"}\n"
    }

    res ++= "}\n"
    res.toString
  }
}
