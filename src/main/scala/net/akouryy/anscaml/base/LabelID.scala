package net.akouryy.anscaml
package base

import scala.collection.mutable

final case class LabelID(name: String)

object LabelID {
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

  def generate(): LabelID = {
    cnt += 1
    LabelID(s"$$tmpLabel$cnt")
  }

  private[this] val cntMap = mutable.Map[LabelID, Int]()

  def generate(label: LabelID): LabelID = {
    val c = cntMap.getOrElse(label, -1) + 1
    cntMap(label) = c
    LabelID(s"${label.name}${suffix(c)}")
  }
}
