package net.akouryy.anscaml
package base

import typ.Typ

import scala.collection.mutable

final case class ID(name: String)

object ID {
  /**
    * `0...26 -> A..Z, 26...26^2+26 -> Aa..Zz, 26^2+26...26^3+26^2+26 -> Aaa..Zzz`
    */
  def suffix(c0: Int): String = {
    assert(c0 >= 0)
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

  private[this] var cnt = -1

  def generate(): ID = {
    cnt += 1
    ID(s"$$t${suffix(cnt)}")
  }

  private[this] val cntMap = mutable.Map[ID, Int]()

  def generate(id: ID): ID = {
    val c = cntMap.getOrElse(id, -1) + 1
    cntMap(id) = c
    ID(s"${id.name}${suffix(c)}")
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
