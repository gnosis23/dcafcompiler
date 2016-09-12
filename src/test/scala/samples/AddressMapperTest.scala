package samples

import org.bohao.decaf.asm.{Register, RegisterType, RelativeMem, AddressMapper}
import org.bohao.decaf.compile.Compiler
import org.bohao.decaf.ir._
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/9/12.
  */
class AddressMapperTest extends FunSpec {
    describe("mapper") {
        it("map1") {
            val mapper = new AddressMapper
            mapper.addArgument("a1")
            mapper.addArgument("a2")
            mapper.addArgument("a3")
            mapper.addArgument("a4")
            mapper.addArgument("a5")
            mapper.addArgument("a6")
            mapper.addArgument("a7")
            mapper.addArgument("a8")
            mapper.addArgument("a9")

            assert(mapper.findAddress(ParamOperand("a1")) == Register(RegisterType.rdi))
            assert(mapper.findAddress(ParamOperand("a3")) == Register(RegisterType.rdx))
            assert(mapper.findAddress(ParamOperand("a6")) == Register(RegisterType.r9))

            assert(mapper.findAddress(ParamOperand("a7")) == RelativeMem(RegisterType.rbp, 16))
            assert(mapper.findAddress(ParamOperand("a8")) == RelativeMem(RegisterType.rbp, 24))
            assert(mapper.findAddress(ParamOperand("a9")) == RelativeMem(RegisterType.rbp, 32))

            val mem = MemoryPointerOperand("a1")
            mapper.addAlloca(Alloca(mem))
            assert(mapper.findAddress(mem) == RelativeMem(RegisterType.rbp, -8))

            val mem2 = MemoryPointerOperand("a2")
            mapper.addAlloca(Alloca(mem2))
            assert(mapper.findAddress(mem2) == RelativeMem(RegisterType.rbp, -16))

            val mem3 = MemoryPointerOperand("a3")
            mapper.addAllocaArray(AllocaArray(mem3, IntOperand(3)))
            assert(mapper.findAddress(mem3) == RelativeMem(RegisterType.rbp, -40))

            val mem4 = MemoryPointerOperand("a3[1]")
            mapper.addGetElement(GetElement(mem4, mem3, IntOperand(1)))
            assert(mapper.findAddress(mem4) == RelativeMem(RegisterType.rbp, -32))
        }
    }
}
