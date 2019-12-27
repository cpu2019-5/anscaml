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
  *  |x   |x   |x   |x   |x   |x
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
        Branch(cm3, ji3, expr3, bi2, tbi4, fbi4) <- f.body.jumps.valuesIterator
        Block(_, Nil, ji1, _) <- f.body.blocks.get(bi2).orElse(???)
        Merge(cm1, _, inputs1, outputID1: XVar, _) <- f.body.jumps.get(ji1).orElse(???)
        if outputID1 == expr3.left &&
           V(outputID1) != expr3.rightVC &&
           !useSets(tbi4).contains(outputID1) &&
           !useSets(fbi4).contains(outputID1)
      } {
        val _ = outputID1 // scala/bug#11175 unused var false positive

        changed = true

        val truInputList = mutable.ListBuffer[MergeInput]()
        val flsInputList = mutable.ListBuffer[MergeInput]()
        val truMergeIndex3 = JumpIndex.generate(ji3)
        val flsMergeIndex3 = JumpIndex.generate(ji3)

        for (MergeInput(bi0, xid0) <- inputs1) {
          val condIndex1 = JumpIndex.generate(ji1)
          val truGlueIndex2 = BlockIndex.generate(bi2)
          val flsGlueIndex2 = BlockIndex.generate(bi2)

          f.body.blocks(bi0) = f.body.blocks(bi0).copy(output = condIndex1)
          f.body.jumps(condIndex1) = Branch(
            cm3, condIndex1, expr3.mapL { l => assert(l == outputID1); xid0 }, bi0, truGlueIndex2,
            flsGlueIndex2,
          )
          f.body.blocks(truGlueIndex2) = Block(truGlueIndex2, Nil, condIndex1, truMergeIndex3)
          f.body.blocks(flsGlueIndex2) = Block(flsGlueIndex2, Nil, condIndex1, flsMergeIndex3)

          truInputList += MergeInput(truGlueIndex2, xid0)
          flsInputList += MergeInput(flsGlueIndex2, xid0)
        }

        f.body.jumps(truMergeIndex3) =
          Merge(cm1, truMergeIndex3, truInputList.toList, outputID1, tbi4)
        f.body.jumps(flsMergeIndex3) =
          Merge(cm1, flsMergeIndex3, flsInputList.toList, outputID1, fbi4)

        f.body.blocks(tbi4) = f.body.blocks(tbi4).copy(input = truMergeIndex3)
        f.body.blocks(fbi4) = f.body.blocks(fbi4).copy(input = flsMergeIndex3)

        f.body.blocks -= bi2
        f.body.jumps --= Set(ji1, ji3)
      }
    }
    changed
  }

}
