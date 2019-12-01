package net.akouryy.anscaml
package knorm

import base._
import net.akouryy.anscaml.knorm.KNorm.IfCmp

object PPrinter {

  sealed trait LetLike

  object LetLike {

    final case class Binding(comment: Comment, entry: Entry, bound: KNorm) extends LetLike

    final case class BindingTuple(comment: Comment, elems: List[Entry], bound: ID) extends LetLike

    final case class BindingRec(
      comment: Comment,
      entry: Entry, args: List[Entry], annot: Set[String],
      bound: KNorm,
    ) extends LetLike

    def unapply(x: Any): Option[(LetLike, KNorm)] = {
      import KNorm._
      x match {
        case KNorm(cmt, Let(entry, bound, kont)) => Some(Binding(cmt, entry, bound), kont)
        case KNorm(cmt, LetTuple(elems, bound, kont)) => Some(BindingTuple(cmt, elems, bound), kont)
        case KNorm(cmt, LetRec(FDef(entry, args, body, annot), kont)) =>
          Some(BindingRec(cmt, entry, args, annot, body), kont)
        case _ => None
      }
    }
  }

  def handle(pp: => pprint.PPrinter): PartialFunction[Any, pprint.Tree] = {
    {
      case LetLike(binding1, kont1) =>
        var bindingsRev = List(binding1)
        var kont = kont1

        while (kont match {
          case LetLike(bindingX, kontX) =>
            kont = kontX
            bindingsRev ::= bindingX
            true
          case _ => false
        }) {}

        pprint.Tree.Apply("Let*", (kont :: bindingsRev).map(pp.treeify).reverseIterator)

      case KNorm(NoComment, kn) => pp.treeify(kn)

      case IfCmp(op, ID(left), ID(right), tru, fls) =>
        pprint.Tree.Apply("IfCmp", Iterator(
          pprint.Tree.Infix(pp.treeify(left), op.toString, pp.treeify(right)),
          pp.treeify(tru),
          pp.treeify(fls),
        ))

    }
  }
}
