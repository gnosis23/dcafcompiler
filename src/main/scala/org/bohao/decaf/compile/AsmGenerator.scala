package org.bohao.decaf.compile

import org.bohao.decaf.asm.RegisterType.{r11, rax, r10}
import org.bohao.decaf.asm._
import org.bohao.decaf.ir.Call
import org.bohao.decaf.ir.Ret
import org.bohao.decaf.ir._

/**
  * Created by bohao on 2016/9/12.
  */
object AsmGenerator {
    val asm = new Code

    def gen(ir : Ir2) : Code = {
        ir.functions.foreach(func => {
            val funcCode = gen(func)
            asm.funcCodes += funcCode
        })

        asm
    }

    def gen(func : IFunction) : FuncCode = {
        val funcCode = new FuncCode(func.name)
        val addressMapper = new AddressMapper

        // map ParamOperand() -> Register
        func.functionType.args.foreach(arg => {
            addressMapper.addArgument(arg._1)
        })

        // map MemoryPointerOperand -> stack position
        func.allocas.foreach {
            case a1 @ Alloca(dest) =>
                addressMapper.addAlloca(a1)
            case a2 @ AllocaArray(dest, size) =>
                addressMapper.addAllocaArray(a2)
        }

        // instructions
        func.blocks.foreach(block => {
            funcCode.add(org.bohao.decaf.asm.Tag(Label(block.name)))
            gen(funcCode, addressMapper, block)
        })
        funcCode.stackSize = addressMapper.stackSize

        funcCode
    }

    def gen(funcCode: FuncCode, mapper: AddressMapper, block: BasicBlock): Unit = {
        block.insts.foreach {
            case Br(b) =>
                funcCode.add(Jmp(Label(b.block.name)))

            case CondBr(cond, thenBody, elseBody) =>
                val condAddr = mapper.findAddress(cond)
                funcCode.add(Mov(condAddr, Register(r10)))
                funcCode.add(Mov(Imm(1), Register(r11)))
                funcCode.add(Cmp(Register(r10), Register(r11)))
                funcCode.add(Jne(Label(elseBody.block.name)))

            case Store(mem, startVal) =>
                val src = mapper.findAddress(startVal)
                val dest = mapper.findAddress(mem)
                (src, dest) match {
                    case (a : Register, _) =>
                        funcCode.add(Mov(src, dest))
                    case (_ , b : Register) =>
                        funcCode.add(Mov(src, dest))
                    case _ =>
                        val t = Register(r10)
                        funcCode.add(Mov(src, t))
                        funcCode.add(Mov(t, dest))
                }

            case Load(dest, mem) =>
                val srcAddr = mapper.findAddress(mem)
                val destAddr = mapper.findAddress(dest)
                (srcAddr, destAddr) match {
                    case (a : Register, _) =>
                        funcCode.add(Mov(srcAddr, destAddr))
                    case (_, b : Register) =>
                        funcCode.add(Mov(srcAddr, destAddr))
                    case _ =>
                        val t = Register(r10)
                        funcCode.add(Mov(srcAddr, t))
                        funcCode.add(Mov(t, destAddr))
                }

            case GetElement(dest, mem, index) =>

            case IAdd(dest, src1, src2) =>
                val t = Register(r10)
                val src1Addr = mapper.findAddress(src1)
                funcCode.add(Mov(src1Addr, t))
                val src2Addr = mapper.findAddress(src2)
                funcCode.add(Add(src2Addr, t))
                val destAddr = mapper.findAddress(dest)
                funcCode.add(Mov(t, destAddr))

            case ISub(dest, src1, src2) =>
            case IMul(dest, src1, src2) =>
            case ITesteq(dest, src1, src2) =>
                val src1Addr = mapper.findAddress(src1)
                val src2Addr = mapper.findAddress(src2)
                funcCode.add(Mov(src1Addr, Register(r10)))
                funcCode.add(Mov(src2Addr, Register(r11)))
                funcCode.add(Cmp(Register(r10), Register(r11)))
                funcCode.add(Mov(Imm(0), Register(r11)))
                funcCode.add(Mov(Imm(1), Register(r10)))
                funcCode.add(Cmove(Register(r10), Register(r11)))
                val destAddr = mapper.findAddress(dest)
                funcCode.add(Mov(Register(r11), destAddr))

            case ITestneq(dest, src1, src2) =>
            case ITestl(dest, src1, src2) =>
            case ITestle(dest, src1, src2) =>
            case ITestg(dest, src1, src2) =>
            case ITestge(dest, src1, src2) =>
            case ICmp(dest, src1) =>

            case Ret(value) =>
                val srcAddr = mapper.findAddress(value)
                funcCode.add(Mov(srcAddr, Register(rax)))

            case Ret0 =>
                funcCode.add(org.bohao.decaf.asm.Ret)
            case Call(retValue, func, args) =>
                var index = 0
                args.foreach(a => {
                    val addr = mapper.findAddress(a)
                    checkStrLiteral(funcCode, addr)
                    index += 1
                    index match {
                        case 1 => funcCode.add(Mov(addr, Register(RegisterType.rdi)))
                        case 2 => funcCode.add(Mov(addr, Register(RegisterType.rsi)))
                        case 3 => funcCode.add(Mov(addr, Register(RegisterType.rdx)))
                        case 4 => funcCode.add(Mov(addr, Register(RegisterType.rcx)))
                        case 5 => funcCode.add(Mov(addr, Register(RegisterType.r8)))
                        case 6 => funcCode.add(Mov(addr, Register(RegisterType.r9)))
                        case _ => funcCode.add(Push(addr))
                    }
                })

                if (func == "printf") {
                    funcCode.add(Mov(Imm(0), Register(rax)))
                }
                funcCode.add(org.bohao.decaf.asm.Call(Func(func)))

                val destAddr = mapper.findAddress(retValue)
                funcCode.add(Mov(Register(rax), destAddr))
                if (index > 6) {
                    for (i <- 6 to index) {
                        funcCode.add(Pop(Register(r10)))
                    }
                }

            case T1(value) =>
            case Assign(dest, value) =>
            case Neg(dest, value) =>
            case Rev(dest, value) =>
        }
    }

    def checkStrLiteral(funcCode: FuncCode, addr: Op) = {
        addr match {
            case lbl @ Label(name) =>
                if (asm.stringLiterals.contains(name)) {
                    lbl.name = asm.stringLiterals.get(name).get
                } else {
                    val size = asm.stringLiterals.size
                    lbl.name = ".str" + size
                    asm.stringLiterals = asm.stringLiterals +
                        (s".str${size+1}" -> name)
                }
            case _ =>
        }
    }
}
