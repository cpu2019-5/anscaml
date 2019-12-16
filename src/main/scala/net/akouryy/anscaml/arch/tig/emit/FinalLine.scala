package net.akouryy.anscaml
package arch.tig
package emit

import asm.FPUFlag
import base._

sealed trait FinalLine {
  def toFinalString: String
}

final case class FinalLabel(label: String, comment: String) extends FinalLine {
  override def toFinalString: String = {
    val base = s"$label:"
    if (comment.nonEmpty) {
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
      case NC => base
      case CM(msg) => f"$base%-69s # $msg"
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
  _acceptValidArgs: => PartialFunction[List[FinalArg], Unit]
) {
  private[this] lazy val acceptValidArgs = _acceptValidArgs

  def isValidWith(args: List[FinalArg]): Boolean = acceptValidArgs.isDefinedAt(args)
}

sealed trait WithFPUFlag extends Product {
  this: FinalInst =>
  val ff: FPUFlag

  override def toString: String = productPrefix + s"${ff.toSuffix}"
}

//noinspection SpellCheckingInspection
object FinalInst {
  def fromUnOp(op: asm.UnOp): FinalInst = op match {
    case asm.Floor => floor
    case asm.Itof => itof
    case asm.FInv(ff) => finv(ff)
    case asm.FSqrt => fsqrt
  }

  def fromBinOpV(op: asm.BinOpV): FinalInst = op match {
    case asm.Sub => sub
    case asm.Div => div
    case asm.Fadd(ff) => fadd(ff)
    case asm.Fsub(ff) => fsub(ff)
    case asm.Fmul(ff) => fmul(ff)
    case asm.Fdiv(ff) => fdiv(ff)
    case asm.FnegCond => fnegcond
  }

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

  case class fadd(ff: FPUFlag) extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  }) with WithFPUFlag

  case class fsub(ff: FPUFlag) extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  }) with WithFPUFlag

  case class fmul(ff: FPUFlag) extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  }) with WithFPUFlag

  case class fdiv(ff: FPUFlag) extends FinalInst({
    case List(_: Reg, _: Reg, _: Reg) =>
  }) with WithFPUFlag

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

  case class finv(ff: FPUFlag) extends FinalInst({
    case List(_: Reg, _: Reg) =>
  }) with WithFPUFlag

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
