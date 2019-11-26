package net.akouryy.anscaml.base

sealed trait IntOrFloat {
  type T
  val typ: Typ
}

object IntOrFloat {
  type Int = Int.type
  type Float = Float.type

  case object Int extends IntOrFloat {
    type T = scala.Int

    val typ = Typ.Int()
  }

  case object Float extends IntOrFloat {
    type T = scala.Float

    val typ = Typ.Float()
  }

}
