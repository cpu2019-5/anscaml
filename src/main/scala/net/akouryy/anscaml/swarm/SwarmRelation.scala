package net.akouryy.anscaml.swarm

sealed trait SwarmRelation

case object SRArray extends SwarmRelation

final case class SRTuple(i: Int) extends SwarmRelation
