package net.akouryy.anscaml
package typ

import syntax.Syntax

object Typing {
  def solve(ast: Syntax): Syntax = {
    Logger.log("TY", "Start")
    val constraints = new Constrainer().constrain(ast)
    val tsb = Unifier.unify(constraints)
    tsb * ast
  }
}
