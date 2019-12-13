package net.akouryy.anscaml
package arch.tig
package optimize

import asm._

import scala.collection.mutable

/**
  * before
  * {{{
  * [1]  [2]  [3]      bi0
  *  |a   |b   |c
  * [[[[merge]]]]      ji1
  *       |d
  *   [5:empty]        bi2
  *       |
  *  [[[if d=0]]]      ji3
  *   |true    |false
  *  [6]      [7]      bi4
  * }}}
  *
  * after
  * {{{
  * [1]       [2]       [3]
  *  |         |         |
  * [if a=0]  [if b=0]  [if c=0]
  *  |t   |f   |t   |f   |t   |f
  * [5A] [5B] [5C] [5D] [5E] [5F]: all empty
  *  |a   |a   |b   |b   |c   |c
  *  *---- ----*---- ----*    |
  *  |    |         |         |
  *  |   [[[[[[[[[merge]]]]]]]]]
  * [merge]         |x
  *  |x            [7]
  * [6]
  * }}}
  */
object DistributeIf {
  def apply(program: Program, useSets: Map[BlockIndex, Set[XVar]]): Boolean = {
    var changed = false
    for (f <- program.functions) {
      for {
        Branch(cm3, ji3, expr, bi2, tbi4, fbi4) <- f.body.jumps.valuesIterator
        Block(_, Nil, ji1, _) <- f.body.blocks.get(bi2).orElse(???)
        Merge(cm1, _, inputs1, outputID1: XVar, _) <- f.body.jumps.get(ji1).orElse(???)
        if outputID1 == expr.left &&
           V(outputID1) != expr.rightVC &&
           !useSets(tbi4).contains(outputID1) &&
           !useSets(fbi4).contains(outputID1)
      } {
        val _ = outputID1 // scala/bug#11175 unused var false positive

        changed = true

        val truInputList = mutable.ListBuffer[(XID, BlockIndex)]()
        val flsInputList = mutable.ListBuffer[(XID, BlockIndex)]()
        val truMergeIndex = JumpIndex.generate(ji3)
        val flsMergeIndex = JumpIndex.generate(ji3)

        for ((xid0, bi0) <- inputs1) {
          val condIndex = JumpIndex.generate(ji1)
          val truGlueIndex = BlockIndex.generate(bi2)
          val flsGlueIndex = BlockIndex.generate(bi2)

          f.body.blocks(bi0) = f.body.blocks(bi0).copy(output = condIndex)
          f.body.jumps(condIndex) = Branch(
            cm3, condIndex, expr.mapL(_ => xid0), bi0, truGlueIndex, flsGlueIndex,
          )
          f.body.blocks(truGlueIndex) = Block(truGlueIndex, Nil, condIndex, truMergeIndex)
          f.body.blocks(flsGlueIndex) = Block(flsGlueIndex, Nil, condIndex, flsMergeIndex)

          truInputList += ((xid0, truGlueIndex))
          flsInputList += ((xid0, flsGlueIndex))
        }

        f.body.jumps(truMergeIndex) =
          Merge(cm1, truMergeIndex, truInputList.toList, XReg.DUMMY, tbi4)
        f.body.jumps(flsMergeIndex) =
          Merge(cm1, flsMergeIndex, flsInputList.toList, XReg.DUMMY, fbi4)

        f.body.blocks(tbi4) = f.body.blocks(tbi4).copy(input = truMergeIndex)
        f.body.blocks(fbi4) = f.body.blocks(fbi4).copy(input = flsMergeIndex)

        f.body.blocks -= bi2
        f.body.jumps --= Set(ji1, ji3)
      }
    }
    changed
  }

}
