package samples

import org.bohao.decaf.asm.RegisterType._
import org.bohao.decaf.asm._
import org.scalatest.FunSpec
import org.bohao.decaf.compile.Compiler

/**
  * Created by bohao on 2016/9/11.
  */
class CodeTest extends FunSpec {
    describe("ir") {
        it("if") {
            val path = getClass.getClassLoader.getResource("code-1.dcaf").toURI.getPath
            val ret = Compiler.code(path)
            val ir = ret._1
            val code = ret._2

            ir.dump()

//            code.funcCodes.foreach(code => code.inst.foreach(println))

            AsmDumper.dump(code)
            assert(code != null)
        }

        it("heart") {
            val path = getClass.getClassLoader.getResource("heart.dcaf").toURI.getPath
            val ret = Compiler.code(path)
//            val ir = ret._1
            val code = ret._2

//            ir.dump()

            AsmDumper.dump(code)
            assert(code != null)
        }

        it("sort") {
            val path = getClass.getClassLoader.getResource("sort.dcaf").toURI.getPath
            val ret = Compiler.code(path)
            val code = ret._2

            AsmDumper.dump(code)
            assert(code != null)
        }

        it("array-len") {
            val path = getClass.getClassLoader.getResource("array-len.dcaf").toURI.getPath
            val ret = Compiler.code(path)
            val code = ret._2

            AsmDumper.dump(code)
            assert(code != null)
        }

        it("array-index") {
            val path = getClass.getClassLoader.getResource("array-index.dcaf").toURI.getPath
            val ret = Compiler.code(path)
            val code = ret._2

            AsmDumper.dump(code)
            assert(code != null)
        }

        it("cond") {
            val path = getClass.getClassLoader.getResource("cond.dcaf").toURI.getPath
            val ret = Compiler.code(path)
            val code = ret._2

            AsmDumper.dump(code)
            assert(code != null)
        }
    }

    describe("code") {
        it("test") {
            val foo = List[Inst](
                Enter(Imm(2)),
                Mov(Register(rdi), RelativeMem(rbp, -8)),

                Mov(RelativeMem(rbp, -8), Register(r10)),
                Add(Imm(3), Register(r10)),
                Mov(Register(r10), RelativeMem(rbp, -16)),

                Mov(RelativeMem(rbp, -16), Register(rax)),
                Leave,
                Ret
            )

            val main = List[Inst](
                Enter(Imm(6))

                ,Call(Func("get_int_035"))
                ,Mov(Register(rax), RelativeMem(rbp, -8))

                ,Mov(RelativeMem(rbp, -8), Register(rdi))
                ,Call(Func("foo"))
                ,Mov(Register(rax), RelativeMem(rbp, -16))
                ,Mov(RelativeMem(rbp, -16), Register(r10))
                ,Mov(Register(r10), RelativeMem(rbp, -24))

                ,Mov(RelativeMem(rbp, -24), Register(r10))
                ,Mov(Imm(15), Register(r11))
                ,Cmp(Register(r10), Register(r11))
                ,Mov(Imm(0), Register(r11))
                ,Mov(Imm(1), Register(r10))
                ,Cmove(Register(r10), Register(r11))
                ,Mov(Register(r11), RelativeMem(rbp, -32))

                ,Mov(RelativeMem(rbp, -32), Register(r10))
                ,Mov(Imm(1), Register(r11))
                ,Cmp(Register(r10), Register(r11))
                ,Je(Label("fifteen"))
            )

            foo.foreach(println)
            main.foreach(println)
        }
    }
}
