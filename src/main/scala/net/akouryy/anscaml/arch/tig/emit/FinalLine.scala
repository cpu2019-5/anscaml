package net.akouryy.anscaml
package arch.tig
package emit

import base._

sealed trait FinalLine {
  def toFinalString: String
}

final case class FinalLabel(label: String, comment: String) extends FinalLine {
  override def toFinalString: String = {
    val base = s"$label:"
    if (comment.nonEmpty && AnsCaml.config.doEmitComment) {
      f"$base%-69s # $comment"
    } else {
      base
    }
  }
}

final case class FinalCommand(comment: Comment, inst: FinalInst, args: List[FinalArg])
  extends FinalLine {

  require(inst.isValidWith(args), (inst, args))

  override def toFinalString: String = {
    val base = f"  $inst%-9s " + args.map(a => f"${a.toFinalString}%9s").mkString(" ")
    comment match {
      case CM(msg) if AnsCaml.config.doEmitComment => f"$base%-69s # $msg"
      case _ => base
    }
  }
}

object FinalCommand {
  def apply(comment: Comment, inst: FinalInst, args: FinalArg*) =
    new FinalCommand(comment, inst, args.toList)
}

sealed abstract class FinalArg(val toFinalString: String)

object FinalArg {

  final case class Reg(r: String) extends FinalArg(r)

  sealed abstract class Imm(dom: Range, i: Int) extends FinalArg(i.toString) {
    require(dom contains i, (dom, i))
  }

  final case class TImm(i: Int) extends Imm(TImm.dom, i)

  object TImm {
    val dom: Range = -(1 << 5) until (1 << 5)
  }

  final case class SImm(i: Int) extends Imm(SImm.dom, i)

  object SImm {
    val dom: Range = -(1 << 15) until (1 << 15)
  }

  final case class UImm(i: Int) extends Imm(UImm.dom, i)

  object UImm {
    val dom: Range = 0 until (1 << 16)
  }

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
  final def isValidWith(args: List[FinalArg]): Boolean = acceptValidArgs.isDefinedAt(args)
}

//noinspection SpellCheckingInspection
object FinalInst {
  def fromUnOp(op: asm.UnOp): FinalInst = op match {
    case asm.Floor => floor
    case asm.Itof => itof
    case asm.Ftoi => ftoi
    case asm.FInv => finv
    case asm.FSqrt => fsqrt
  }

  def fromBinOpV(op: asm.BinOpV): FinalInst = op match {
    case asm.Sub => sub
    case asm.Div => div
    case asm.Fadd => fadd
    case asm.Fsub => fsub
    case asm.Fmul => fmul
    case asm.Fdiv => fdiv
    case asm.FnegCond => fnegcond
    case asm.FaddAbs => faddabs
  }

  def vFromBinOpVC(op: asm.BinOpVC): FinalInst = op match {
    case asm.Add => add
    case asm.Sha => sha
    case asm.Band => band
    case asm.Bor => or
  }

  def cFromBinOpVC(op: asm.BinOpVC): FinalInst = op match {
    case asm.Add => addi
    case asm.Sha => shai
    case asm.Band => bandi
    case asm.Bor => ????(or)
  }

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

  case object faddabs extends FinalInst({
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
    case List(_: Reg, _: Reg, _: SImm) =>
  })

  case object shai extends FinalInst({
    case List(_: Reg, _: Reg, _: TImm) =>
  })

  case object bandi extends FinalInst({
    case List(_: Reg, _: Reg, _: UImm) =>
  })

  case object orhi extends FinalInst({
    case List(_: Reg, _: Reg, _: UImm) =>
  })

  case object load extends FinalInst({
    case List(_: Reg, _: Reg, _: SImm) =>
  })

  case object store extends FinalInst({
    case List(_: Reg, _: SImm, _: Reg) =>
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
    case List(_: Reg, _: TImm, Label(LRel, _)) =>
  })

  case object jgti extends FinalInst({
    case List(_: Reg, _: TImm, Label(LRel, _)) =>
  })

  case object jlti extends FinalInst({
    case List(_: Reg, _: TImm, Label(LRel, _)) =>
  })

}
