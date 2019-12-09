package net.akouryy.anscaml.arch.tig
package emit

sealed trait FinalLine {
  def toFinalString: String
}

final case class FinalLabel(label: String, comment: String) extends FinalLine {
  override def toFinalString: String = {
    val base = s"$label:"
    if (comment.nonEmpty) {
      f"$base%-70s# $comment"
    } else {
      base
    }
  }
}

final case class FinalCommand(comment: String, inst: FinalInst, args: List[FinalArg])
  extends FinalLine {

  require(inst.isValidWith(args), (inst, args))

  override def toFinalString: String = {
    val base = f"  $inst%-9s " + args.map(a => f"${a.toFinalString}%9s").mkString(" ")
    if (comment.nonEmpty) {
      f"$base%-70s# $comment"
    } else {
      base
    }
  }
}

object FinalCommand {
  def apply(comment: String, inst: FinalInst, args: FinalArg*) =
    new FinalCommand(comment, inst, args.toList)
}

sealed abstract class FinalArg(val toFinalString: String)

object FinalArg {

  final case class Reg(r: String) extends FinalArg(r)

  final case class Imm(i: Int) extends FinalArg(i.toString)

  final case class Label(op: LabelOp, l: String) extends FinalArg(op.toFinalString + l)

  sealed abstract class LabelOp(val toFinalString: String)

  case object LHigh extends LabelOp("high:")

  case object LLow extends LabelOp("low:")

  case object LRel extends LabelOp("rel:")

  case object LAbs extends LabelOp("")

}

sealed abstract class FinalInst(
  private[this] val acceptValidArgs: PartialFunction[List[FinalArg], Unit]
) {
  def isValidWith(args: List[FinalArg]): Boolean = acceptValidArgs.isDefinedAt(args)
}

//noinspection SpellCheckingInspection
object FinalInst {
  val fromUnOp: Map[asm.UnOp, FinalInst] = Map(
    asm.Floor -> floor,
    asm.Itof -> itof,
  )

  val fromBinOpV: Map[asm.BinOpV, FinalInst] = Map(
    asm.Sub -> sub,
    asm.Div -> div,
    asm.Fadd -> fadd,
    asm.Fsub -> fsub,
    asm.Fmul -> fmul,
    asm.Fdiv -> fdiv,
    asm.FnegCond -> fnegcond,
  )

  val vFromBinOpVC: Map[asm.BinOpVC, FinalInst] = Map(
    asm.Add -> add,
    asm.Sha -> sha,
    asm.Band -> band,
    asm.Bor -> or,
  )

  val cFromBinOpVC: Map[asm.BinOpVC, FinalInst] = Map(
    asm.Add -> addi,
    asm.Sha -> shai,
    asm.Band -> bandi,
  )

  val negJumpFromCmpOpV: Map[asm.CmpOpV, FinalInst] = Map(
    asm.FLe -> fjgt,
  )

  val negVJumpFromCmpOpVC: Map[asm.CmpOpVC, FinalInst] = Map(
    asm.Eq -> jne,
    asm.Le -> jgt,
  )

  val negCJumpFromCmpOpVC: Map[asm.CmpOpVC, FinalInst] = Map(
    asm.Eq -> jnei,
    asm.Le -> jgti,
  )

  import FinalArg._

  case object j extends FinalInst({
    case List(Label(LAbs, _)) =>
  })

  case object jal extends FinalInst({
    case List(Label(LAbs, _)) =>
  })

  case object add extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object sub extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object div extends FinalInst({ // TODO: remove
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object sha extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object band extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object or extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object fadd extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object fsub extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object fmul extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object fdiv extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object fnegcond extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object fsqrt extends FinalInst({
    case List(_: Reg, _: Reg) =>
  })

  case object floor extends FinalInst({
    case List(_: Reg, _: Reg) =>
  })

  case object itof extends FinalInst({
    case List(_: Reg, _: Reg) =>
  })

  case object ftoi extends FinalInst({
    case List(_: Reg, _: Reg) =>
  })

  case object finv extends FinalInst({
    case List(_: Reg, _: Reg) =>
  })

  case object loadreg extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  })

  case object jr extends FinalInst({
    case List(_: Reg) =>
  })

  case object read extends FinalInst({
    case List(_: Reg) =>
  })

  case object write extends FinalInst({
    case List(_: Reg) =>
  })

  case object nop extends FinalInst({
    case List() =>
  })

  case object exit extends FinalInst({
    case List() =>
  })

  case object addi extends FinalInst({
    case List(_: Reg, _: Reg, _: Imm) =>
  })

  case object shai extends FinalInst({
    case List(_: Reg, _: Reg, _: Imm) =>
  })

  case object bandi extends FinalInst({
    case List(_: Reg, _: Reg, _: Imm) =>
  })

  case object orhi extends FinalInst({
    case List(_: Reg, _: Reg, _: Imm) =>
  })

  case object load extends FinalInst({
    case List(_: Reg, _: Reg, _: Imm) =>
  })

  case object store extends FinalInst({
    case List(_: Reg, _: Imm, _: Reg) =>
  })

  case object jne extends FinalInst({
    case List(_: Reg, _: Reg, Label(LRel, _)) =>
  })

  case object jgt extends FinalInst({
    case List(_: Reg, _: Reg, Label(LRel, _)) =>
  })

  case object fjgt extends FinalInst({
    case List(_: Reg, _: Reg, Label(LRel, _)) =>
  })

  case object jnei extends FinalInst({
    case List(_: Reg, _: Imm, Label(LRel, _)) =>
  })

  case object jgti extends FinalInst({
    case List(_: Reg, _: Imm, Label(LRel, _)) =>
  })

  case object jlti extends FinalInst({
    case List(_: Reg, _: Imm, Label(LRel, _)) =>
  })

}
