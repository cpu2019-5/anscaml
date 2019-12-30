package net.akouryy.anscaml
package arch.tig

import scala.collection.mutable

class GraphDrawer {
  private[this] val res = new StringBuilder

  private[this] def unsafeEscape(str: String): String = {
    str.replaceAll("&", "&nbsp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")
      .replaceAll("\"", "&quot;")
  }

  private[this] def condStr(cond: asm.Branch.Cond) = {
    s"${cond.left} ${
      cond.opBase match {
        case asm.Eq => "=="
        case asm.Le => "<="
        case asm.FLe => "<=."
      }
    } ${cond.rightVC}"
  }

  private[this] val forColors = mutable.Map[asm.JumpIndex, Int]()

  //noinspection SpellCheckingInspection
  def apply(p: asm.Program): String = {
    forColors.clear()
    res.clear()
    res ++=
    """digraph Program {
      |graph [fontname = "Monaco", fontsize = 12, ranksep = 0.5];
      |node [shape = box, fontname = "Monaco", fontsize = 11; colorscheme = pastel19];
      |edge [fontname = "Monaco", fontsize = 11; colorscheme = pastel19];
      |""".stripMargin

    for (f <- p.functions) {
      res ++=
      s"""subgraph cluster_${Math.abs(f.hashCode)} {
         |label="${f.name}";
         |color=green;
         |""".stripMargin

      for (j <- f.body.jumps.valuesIterator) {
        res ++= (j match {
          case asm.StartFun(_, i, ob) =>
            val args = f.args.mkString(", ")
            s"""$i[label = "StartFun.${i.indexString}"; shape = component];
               |$i -> $ob [label = "($args)"];
               |""".stripMargin
          case asm.Return(_, i, v, ib) =>
            s"""$i[label = "Return.${i.indexString}"; shape = lpromoter];
               |$ib -> $i [label="$v"];
               |""".stripMargin
          case asm.Branch(_, i, cond, ib, tob, fob) =>
            s"""$i[
               |  label = "Branch.${i.indexString}\n${condStr(cond)}";
               |  shape = trapezium; style = rounded;
               |];
               |$ib -> $i;
               |$i -> $tob [label=true];
               |$i -> $fob [label=false];
               |""".stripMargin
          case asm.Merge(_, i, inputs, v, ob) =>
            val inputEdges =
              inputs.map(ib => s"""${ib.bi} -> $i [label="${ib.xid}"];""").mkString
            s"""$i[label = "Merge.${i.indexString}"; shape = invtrapezium; style = rounded];
               |$inputEdges
               |$i -> $ob [label="$v"];
               |""".stripMargin
          case asm.ForLoopTop(_, i, cond, negated, merges, input, loopBottom, body, kont) =>
            forColors(i) = forColors.size % 5 + 1
            val neg = if (negated) "NOT " else ""
            val loopVars = merges.map(_.loop).mkString(", ")
            s"""$i[label = "For.${i.indexString}\n$neg${condStr(cond)}\n$loopVars"; shape = house;
               |   style = filled; fillcolor = ${forColors(i)}];
               |$input -> $i [label = "${merges.map(_.in).mkString(", ")}"];
               |$i -> $body [label = "true"];
               |$loopBottom -> $i [constraint = false; color = ${forColors(i)}];
               |$i -> $kont [label = "false"]
               |""".stripMargin
          case asm.ForLoopBottom(_, i, input, loopTop, merges) =>
            s"""$i[label = "ForEnd.${i.indexString} [${loopTop.indexString}]"; shape = invhouse;
               |   style = filled; fillcolor = ${forColors(loopTop)}];
               |$input -> $i [label = "${merges.map(_.upd).mkString(", ")}"];
               |""".stripMargin
        })
      }

      for (asm.Block(i, lines, _, _) <- f.body.blocks.valuesIterator) {
        if (lines.isEmpty) {
          res ++= s"""$i [label = "$i\\l(0行)"]""" + "\n"
        } else {
          val ls = lines.grouped(if (i == f.body.blocks.firstKey) 30 else 20).toList
          val linesStr = ls.map { lg =>
            """<td align="left" balign="left" valign="top">""" +
            lg.map(
              l => s"${l.dest} = ${unsafeEscape(l.inst.toBriefString)}"
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
