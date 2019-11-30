package net.akouryy.anscaml
package base

import typ.Typ

import scala.collection.mutable

final case class ID(name: String)

object ID {
  private[this] var cnt = -1

  def generate(): ID = {
    cnt += 1
    ID(s"tmp$cnt")
  }

  private[this] var cntMap = mutable.Map[ID, Int]()

  def generate(id: ID): ID = {
    val c = cntMap.getOrElse(id, -1) + 1
    cntMap(id) = c
    ID(s"${id.name}__$c")
  }
}

final case class Entry(name: ID, typ: Typ)

object Entry {
  def generate(): Entry = generate(ID.generate())

  def generate(name: String): Entry = generate(ID(name))

  def generate(id: ID): Entry = Entry(id, Typ.generateTypVar())
}
