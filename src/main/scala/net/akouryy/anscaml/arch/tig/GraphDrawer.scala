package net.akouryy.anscaml
package arch.tig

class GraphDrawer {
  private[this] val res = new StringBuilder

  private[this] def unsafeEscape(str: String): String = {
    str.replaceAll("&", "&nbsp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")
      .replaceAll("\"", "&quot;")
  }

  //noinspection SpellCheckingInspection
  def apply(p: asm.Program): String = {
    res.clear()
    res ++=
    """digraph Program {
      |graph [fontname = "Monaco", fontsize = 12, ranksep = 0.5];
      |node [shape = box, fontname = "Monaco", fontsize = 11];
      |edge [fontname = "Monaco", fontsize = 11];
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
            val args = f.args.mkString(", ")
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
          val ls = lines.grouped(10).toList
          val linesStr = ls.map { lg =>
            """<td align="left" balign="left" valign="top">""" +
            lg.map(
              l => s"${l.dest} = ${unsafeEscape(l.inst.toString)}"
            ).mkString("<br/>") +
            """</td>"""
          }.mkString
          res ++=
          s"""$i [shape = plain; label = <
             |<table border="0" cellborder="1" cellspacing="0">
             |  <tr><td colspan="${ls.size}">$i (${lines.size}行)</td></tr>
             |  <tr>$linesStr</tr>
             |</table>
             |>]""".stripMargin
        }
      }

      res ++= s"}\n"
    }

    res ++= "}\n"
    res.toString
  }
}
