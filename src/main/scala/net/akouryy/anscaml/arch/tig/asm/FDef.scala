package net.akouryy.anscaml
package arch.tig.asm

final case class FDef(name: String, args: List[XID], body: Chart, typ: Fn, info: FDefInfo)

final case class FDefInfo(isLeaf: Boolean)

