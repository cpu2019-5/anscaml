package net.akouryy.anscaml
package knorm

import base._
import net.akouryy.anscaml.knorm.KNorm.IfCmp

object PPrinter {

  sealed trait LetLike

  object LetLike {

    final case class Binding(entry: Entry, bound: KNorm) extends LetLike

    final case class BindingTuple(elems: List[Entry], bound: ID) extends LetLike

    final case class BindingRec(entry: Entry, args: List[Entry], noInline: Boolean, bound: KNorm)
      extends LetLike

    def unapply(x: Any): Option[(LetLike, KNorm)] = {
      import KNorm._
      x match {
        case Let(entry, bound, kont) => Some(Binding(entry, bound), kont)
        case LetTuple(elems, bound, kont) => Some(BindingTuple(elems, bound), kont)
        case LetRec(FDef(entry, args, body, noInline), kont) =>
          Some(BindingRec(entry, args, noInline, body), kont)
        case _ => None
      }
    }
  }

  def handle(pp: => pprint.PPrinter): PartialFunction[Any, pprint.Tree] = {
    {
      case KNorm(NoComment, kn) => pp.treeify(kn)

      case IfCmp(op, ID(left), ID(right), tru, fls) =>
        pprint.Tree.Apply("IfCmp", Iterator(
          pprint.Tree.Infix(pp.treeify(left), op.toString, pp.treeify(right)),
          pp.treeify(tru),
          pp.treeify(fls),
        ))

      case LetLike(binding1, kont1) =>
        var bindingsRev = List(binding1)
        var kont = kont1

        while (kont match {
          case KNorm(NoComment, LetLike(bindingX, kontX)) =>
            kont = kontX
            bindingsRev ::= bindingX
            true
          case _ => false
        }) {}

        pprint.Tree.Apply("Let*", (kont :: bindingsRev).map(pp.treeify).reverseIterator)
    }
  }
}
