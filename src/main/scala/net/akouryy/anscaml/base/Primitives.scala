package net.akouryy.anscaml.base

import net.akouryy.anscaml.typ.Typ

sealed trait Primitives {
  type T
  val typ: Typ
}

object Primitives {

  sealed trait IntOrFloatOrBool extends Primitives

  sealed trait IntOrFloat extends IntOrFloatOrBool

  type Int = Int.type
  type Float = Float.type
  type Bool = Bool.type

  case object Int extends Primitives with IntOrFloat {
    type T = scala.Int

    val typ = Typ.Int(Typ.Lit.All())

    implicit val intSingleton: Int = this
  }

  case object Float extends Primitives with IntOrFloat {
    type T = scala.Float

    val typ = Typ.Float(Typ.Lit.All())
    implicit val floatSingleton: Float = this
  }

  case object Bool extends Primitives with IntOrFloatOrBool {
    type T = Boolean

    val typ = Typ.Bool(Typ.Lit.All())
    implicit val boolSingleton: Bool = this
  }

}
