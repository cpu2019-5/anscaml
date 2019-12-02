package net.akouryy.anscaml
package base

import typ.Typ

import scala.collection.mutable

final case class ID(name: String)

object ID {
  private[this] def suffix(c0: Int) = {
    assert(c0 >= 0)
    var c = c0
    val res = new StringBuilder
    while (c > 0) {
      val start = if (c >= 26) 'a' else 'A'
      res += (start + c % 26).toChar
      c /= 26
    }
    res.reverseInPlace().toString
  }

  private[this] var cnt = -1

  def generate(): ID = {
    cnt += 1
    ID(s"$$tmp$cnt")
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
