package net.akouryy.anscaml
package arch.tig
package optimize

import asm._

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
  def apply(program: Program, useSets: Map[BlockIndex, Set[AVar]]): Boolean = {
    var changed = false
    for (f <- program.functions) {
      for {
        Condition(ji3, op, left, right, bi2, tbi4, fbi4) <- f.body.jumps.valuesIterator
        Block(_, Nil, ji1, _) <- f.body.blocks.get(bi2).orElse(???)
        Merge(_, inputs1, outputID1: AVar, _) <- f.body.jumps.get(ji1).orElse(???)
        if outputID1 == left &&
           !useSets.get(tbi4).exists(_ contains outputID1) &&
           !useSets.get(fbi4).exists(_ contains outputID1)
      } {
        val _ = outputID1 // scala/bug#11175 unused var false positive

        changed = true

        var truInputList = List[(AID, BlockIndex)]()
        var flsInputList = List[(AID, BlockIndex)]()
        val truMergeIndex = JumpIndex.generate(ji3)
        val flsMergeIndex = JumpIndex.generate(ji3)

        for ((aid0, bi0) <- inputs1) {
          val condIndex = JumpIndex.generate(ji1)
          val truGlueIndex = BlockIndex.generate(bi2)
          val flsGlueIndex = BlockIndex.generate(bi2)

          f.body.blocks(bi0) = f.body.blocks(bi0).copy(output = condIndex)
          f.body.jumps(condIndex) = Condition(
            condIndex, op, aid0, right, bi0, truGlueIndex, flsGlueIndex,
          )
          f.body.blocks(truGlueIndex) = Block(truGlueIndex, Nil, condIndex, truMergeIndex)
          f.body.blocks(flsGlueIndex) = Block(flsGlueIndex, Nil, condIndex, flsMergeIndex)

          truInputList ::= (aid0, truGlueIndex)
          flsInputList ::= (aid0, flsGlueIndex)
        }

        f.body.jumps(truMergeIndex) = Merge(truMergeIndex, truInputList, AReg.REG_DUMMY, tbi4)
        f.body.jumps(flsMergeIndex) = Merge(flsMergeIndex, flsInputList, AReg.REG_DUMMY, fbi4)

        f.body.blocks(tbi4) = f.body.blocks(tbi4).copy(input = truMergeIndex)
        f.body.blocks(fbi4) = f.body.blocks(fbi4).copy(input = flsMergeIndex)

        f.body.blocks -= bi2
        f.body.jumps --= Set(ji1, ji3)
      }
    }
    changed
  }

}
