package net.akouryy.anscaml

package object base {
  val pprinter: pprint.PPrinter = pprint.copy(
    additionalHandlers = {
      case true => pprint.Tree.Literal("true")
      case false => pprint.Tree.Literal("false")
      case Nil => pprint.Tree.Literal("Nil")

      case typ.Typ.TypVar(v) => pprint.Tree.Literal(s"$$_$v")
    }
  )
}
