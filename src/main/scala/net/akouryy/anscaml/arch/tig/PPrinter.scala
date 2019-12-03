package net.akouryy.anscaml
package arch.tig

import asm._

object PPrinter {

  def handle(pp: => pprint.PPrinter): PartialFunction[Any, pprint.Tree] = {
    {
      case Program(gcSize, tyEnv @ _, functions) =>
        pprint.Tree.Apply("Program", List(pp.treeify(gcSize), pp.treeify(functions)).iterator)

      case BlockIndex(i) => pprint.Tree.Literal(s"B-$i")
      case JumpIndex(i) => pprint.Tree.Literal(s"J-$i")

      case c: Chart =>
        pprint.Tree.Apply("Chart", List(pp.treeify(c.blocks), pp.treeify(c.jumps)).iterator)
    }
  }
}
