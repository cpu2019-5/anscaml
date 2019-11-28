package net.akouryy.anscaml.base

import net.akouryy.anscaml.typ.Typ

sealed trait Primitives {
  type T
}

object Primitives {

  sealed trait IFB extends Primitives {
    val typ: Typ
  }

  sealed trait IF extends IFB

  type PInt = PInt.type
  type PFloat = PFloat.type
  type PBool = PBool.type

  case object PInt extends Primitives with IF {
    type T = scala.Int

    val typ: Typ.Prim[PInt] = Typ.IntAll

    implicit val intSingleton: PInt = this
  }

  case object PFloat extends Primitives with IF {
    type T = scala.Float

    val typ: Typ.Prim[PFloat] = Typ.FloatAll
    implicit val floatSingleton: PFloat = this
  }

  case object PBool extends Primitives with IFB {
    type T = Boolean

    val typ: Typ.Prim[PBool] = Typ.BoolAll
    implicit val boolSingleton: PBool = this
  }

}
