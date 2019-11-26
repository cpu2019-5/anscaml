package net.akouryy.anscaml.base

final case class ID(name: String)

object ID {
  private[this] var cnt = -1

  def generate(): ID = {
    cnt += 1
    ID(s"tmp$cnt")
  }
}

final case class Entry(name: ID, typ: Typ)

object Entry {
  def generate(): Entry = generate(ID.generate())

  def generate(name: String): Entry = generate(ID(name))

  def generate(id: ID): Entry = Entry(id, Typ.generateTypVar())
}
