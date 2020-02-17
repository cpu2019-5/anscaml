package net.akouryy.anscaml
package base

import typ.Typ

import scala.collection.mutable

final case class ID(str: String)

object ID {
  /**
    * `0...26 -> A..Z, 26...26^2+26 -> Aa..Zz, 26^2+26...26^3+26^2+26 -> Aaa..Zzz`
    */
  def suffix(c0: Int, allowEmptySuffix: Boolean = false): String = {
    assert(c0 >= 0)
    if (c0 == 0 && allowEmptySuffix) return ""
    var c = c0
    var doNext = true
    val res = new StringBuilder
    while (doNext) {
      doNext = c >= 26
      val start = if (doNext) 'a' else 'A'
      res += (start + c % 26).toChar
      c = c / 26 - 1
    }
    res.reverseInPlace().toString
  }

  private[this] var tmpCnt = -1

  def generate(): ID = {
    tmpCnt += 1
    ID(s"$$${suffix(tmpCnt)}")
  }

  private[this] val cntMap = mutable.Map[String, Int]()

  private[this] val TempRegex = """^\$[A-Z][a-z]*$""".r
  private[this] val SuffixedRegex = """^([\w$]+)[A-Z][a-z]*$""".r

  def generate(base: String, allowEmptySuffix: Boolean = false): ID = {
    val str = base match {
      case TempRegex() => base
      case SuffixedRegex(str) => str
      case _ => base
    }
    val c = cntMap.getOrElse(str, -1) + 1
    cntMap(str) = c
    ID(s"${str}${suffix(c, allowEmptySuffix)}")
  }

  //noinspection SpellCheckingInspection
  object Special {
    val SPECIALIZE_ADDR = "$addr"
    val EMIT_ARRAY_BODY = "$arb"
    val EMIT_ARRAY_END = "$are"
    val ASM_EXIT_FUN = "$exit"
    val EXTERNAL_PREFIX = "$ext_"
    val KN_FLOAT = "$f"
    val GC_INSTANCE = "$gci"
    val KO_GENERAL_LOOP_VAR = "$gl"
    val GC_VAL = "$gcv"
    val KN_INT = "$i"
    val ASM_F_INV = "$inv"
    val RA_LINK_REG = "$lr"
    val KO_LOOP_VAR = "$loop"
    val SPECIALIZE_LTC_ELEM = "$ltce"
    val SPECIALIZE_LTC_INDEX = "$ltci"
    val KO_UPD_VAR = "$lup"
    val MAIN = "$main"
    val ASM_NOT = "not"
    val EMIT_SELECT_FLS = "$slf"
    val EMIT_SELECT_END = "$sle"
  }

}

final case class Entry(name: ID, typ: Typ) {
  def toPair: (ID, Typ) = Entry.unapply(this).get
}

object Entry {
  def generate(): Entry = generate(ID.generate())

  def generate(name: String): Entry = generate(ID(name))

  def generate(id: ID): Entry = Entry(id, Typ.generateTypVar())
}
