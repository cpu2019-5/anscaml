package net.akouryy.anscaml
package knorm

import base._
import KNorm._

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
      x match {
        case KNorm(cmt, Let(entry, bound, kont)) => Some(Binding(cmt, entry, bound), kont)
        case KNorm(cmt, LetTuple(elems, bound, kont)) => Some(BindingTuple(cmt, elems, bound), kont)
        case KNorm(cmt, LetRec(FDef(entry, args, body, annot), kont)) =>
          Some(BindingRec(cmt, entry, args, annot, body), kont)
        case _ => None
      }
    }
  }

  sealed trait CLetLike

  object CLetLike {

    final case class Binding(comment: Comment, entry: Entry, bound: KClosed) extends CLetLike

    final case class BindingTuple(comment: Comment, elems: List[Entry], bound: ID) extends CLetLike

    final case class BindingClosure(
      comment: Comment,
      entry: Entry, fn: LabelID, captured: List[ID],
    ) extends CLetLike

    def unapply(x: Any): Option[(CLetLike, KClosed)] = {
      x match {
        case KClosed(cmt, CLet(entry, bound, kont)) => Some((Binding(cmt, entry, bound), kont))
        case KClosed(cmt, CLetTuple(elems, bound, kont)) =>
          Some((BindingTuple(cmt, elems, bound), kont))
        case KClosed(cmt, CLetClosure(entry, fn, captured, kont)) =>
          Some((BindingClosure(cmt, entry, fn, captured), kont))
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
      case CLetLike(binding1, kont1) =>
        var bindingsRev = List(binding1)
        var kont = kont1
        while (kont match {
          case CLetLike(bindingX, kontX) =>
            kont = kontX
            bindingsRev ::= bindingX
            true
          case _ => false
        }) {}
        pprint.Tree.Apply("CLet*", (kont :: bindingsRev).map(pp.treeify).reverseIterator)

      case KNorm(NoComment, kn) => pp.treeify(kn)
      case KClosed(NoComment, kc) => pp.treeify(kc)

      case IfCmp(op, ID(left), ID(right), tru, fls) =>
        pprint.Tree.Apply("IfCmp", Iterator(
          pprint.Tree.Infix(pp.treeify(left), op.toString, pp.treeify(right)),
          pp.treeify(tru),
          pp.treeify(fls),
        ))
    }
  }
}
