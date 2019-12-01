package net.akouryy.anscaml
package syntax

import base._
import net.akouryy.anscaml.knorm.KNorm.IfCmp

object PPrinter {

  sealed trait Binding

  object Binding {

    final case class Bind(entry: Entry, bound: Syntax) extends Binding

    final case class BTuple(elems: List[Entry], bound: Syntax) extends Binding

    final case class BRec(
      entry: Entry, args: List[Entry], noInline: Boolean, bound: Syntax,
    ) extends Binding

    def unapply(x: Any): Option[(Binding, Syntax)] = {
      import Syntax._
      x match {
        case Let(entry, bound, kont) => Some(Bind(entry, bound), kont)
        case LetTuple(elems, bound, kont) => Some(BTuple(elems, bound), kont)
        case LetRec(FDef(entry, args, body, noInline), kont) =>
          Some(BRec(entry, args, noInline, body), kont)
        case _ => None
      }
    }
  }

  def handle(pp: => pprint.PPrinter): PartialFunction[Any, pprint.Tree] = {
    {
      case Binding(binding1, kont1) =>
        var bindingsRev = List(binding1)
        var kont = kont1

        while (kont match {
          case Binding(bindingX, kontX) =>
            kont = kontX
            bindingsRev ::= bindingX
            true
          case _ => false
        }) {}

        pprint.Tree.Apply("Let*", (kont :: bindingsRev).map(pp.treeify).reverseIterator)

      case IfCmp(op, ID(left), ID(right), tru, fls) =>
        pprint.Tree.Apply("IfCmp", Iterator(
          pprint.Tree.Infix(pp.treeify(left), op.toString, pp.treeify(right)),
          pp.treeify(tru),
          pp.treeify(fls),
        ))

    }
  }
}
