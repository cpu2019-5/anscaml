package net.akouryy.anscaml

import syntax.{BinOp, CmpOp}
import typ.{Lit, Typ}

package object base {
  val pprinter: pprint.PPrinter = pprint.copy(
    additionalHandlers =
      identity[PartialFunction[Any, pprint.Tree]]({
        case true => pprint.Tree.Literal("true")
        case false => pprint.Tree.Literal("false")
        case Nil => pprint.Tree.Literal("Nil")

        case Typ.TypVar(v) => pprint.Tree.Literal(s"$$_$v")
        case Typ.BoolAll => pprint.Tree.Lazy(_ => Iterator(s"TBoolAll"))
        case Typ.IntAll => pprint.Tree.Lazy(_ => Iterator(s"TIntAll"))
        case Typ.FloatAll => pprint.Tree.Lazy(_ => Iterator(s"TFloatAll"))
        case Lit.Var(v) => pprint.Tree.Literal(s"@_$v")

        case op: BinOp => pprint.Tree.Lazy(_ => Iterator(op.toString))
        case op: CmpOp => pprint.Tree.Lazy(_ => Iterator(op.toString))

        case Entry(name, typ) =>
          pprint.Tree.Apply("Entry", Iterator(
            pprint.Tree.Infix(pprinter.treeify(name.name), ":", pprinter.treeify(typ))
          ))
      }) orElse syntax.PPrinter.handle(pprinter) orElse knorm.PPrinter.handle(pprinter),

    defaultWidth = 130,
  )

  implicit class RichList[A](val list: List[A]) extends AnyVal {
    def zipMap[B, C](that: IterableOnce[B])(fn: (A, B) => C): List[C] =
      list.zip(that).map(fn.tupled)
  }

}
