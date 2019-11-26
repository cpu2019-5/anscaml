package net.akouryy.anscaml

package object base {
  val pprinter: pprint.PPrinter = pprint.copy(
    additionalHandlers = {
      case true => pprint.Tree.Literal("true")
      case false => pprint.Tree.Literal("false")
      case Nil => pprint.Tree.Literal("Nil")
    }
  )
}
