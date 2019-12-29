package net.akouryy.anscaml
package arch.tig

import asm.XVar
import base._
import swarm.SwarmIndex

import scala.collection.mutable

class TigContext {
  val swarmIndices: mutable.Map[XVar, SwarmIndex] = mutable.Map[XVar, SwarmIndex]()

  def generateXVar(str: String, sameSwarm: Option[XVar]): XVar = {
    val xv = XVar.generate(str)
    sameSwarm.flatMap(swarmIndices.get).foreach(swarmIndices(xv) = _)
    xv
  }
}
