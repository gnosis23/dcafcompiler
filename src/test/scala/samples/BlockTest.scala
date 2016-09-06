package samples

import org.bohao.decaf.compile.Compiler
import org.bohao.decaf.ir.BasicBlock
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/9/6.
  */
class BlockTest extends FunSpec{
    describe("block") {
        it("rename") {
            var block: BasicBlock = null
            block = BasicBlock.create("loop", null)
            block = BasicBlock.create("loop", null)
            assert(block.name == "loop1")

            block = BasicBlock.create("loop", null)
            assert(block.name == "loop2")
        }
    }
}
