package org.bohao.decaf.asm

import org.bohao.decaf.asm.RegisterType._
import org.bohao.decaf.ir._

/**
  * Created by bohao on 2016/9/12.
  */
class AddressMapper() {
    /**
      * memory location diagram
      *
      * --------------------------
      * 8n + 16(%rbp) argument n
      * ...
      * 16(%rbp) argument 7
      * --------------------------
      * return address
      * previous rbp
      * --------------------------
      * -8(%rbp) argument 1
      * ....
      * -8m(%rbp) argument m < 7
      *
      */
    var argumentCount = 0

    var stackSize = 0

    var mapper = Map[Operand, Op]()

    def findAddress(op : Operand) : Op = {
        val ret = mapper.get(op)
        ret match {
            case None => op match {
                case t @ TempVarOperand(id) =>
                    addTemp(t)
                case IntOperand(v) => Imm(v)
                case StrOperand(str) => ImmStr(str)
                case _ => throw new Error("symbol error " + op)
            }
            case Some(x) => x
        }
    }

    /**
      * only int type
      *
      * @param name argument name
      */
    def addArgument(name : String): Unit = {
        argumentCount += 1

        if (argumentCount == 1) {
            mapper = mapper + (ParamOperand(name) -> Register(rdi))
        }
        else if (argumentCount == 2) {
            mapper = mapper + (ParamOperand(name) -> Register(rsi))
        }
        else if (argumentCount == 3) {
            mapper = mapper + (ParamOperand(name) -> Register(rdx))
        }
        else if (argumentCount == 4) {
            mapper = mapper + (ParamOperand(name) -> Register(rcx))
        }
        else if (argumentCount == 5) {
            mapper = mapper + (ParamOperand(name) -> Register(r8))
        }
        else if (argumentCount == 6) {
            mapper = mapper + (ParamOperand(name) -> Register(r9))
        }
        else {
            mapper = mapper + (ParamOperand(name) -> RelativeMem(rbp, 8 * argumentCount - 40))
        }
    }

    def addAlloca(x : Alloca): Unit = {
        stackSize += 8
        mapper = mapper + (x.dest -> RelativeMem(rbp, -stackSize))
    }

    def addAllocaArray(x : AllocaArray): Unit = {
        stackSize += 8 * x.size.asInstanceOf[IntOperand].value
        mapper = mapper + (x.dest -> RelativeMem(rbp, -stackSize))
    }

    def addGetElement(x : GetElement): Unit = {
        val addr = mapper.get(x.mem).get.asInstanceOf[RelativeMem]
        mapper = mapper + (x.dest -> RelativeMem(rbp, addr.offset +
            8 * x.index.asInstanceOf[IntOperand].value))
    }

    def addTemp(x : TempVarOperand): Op = {
        stackSize += 8
        val pos = RelativeMem(rbp, -stackSize)
        mapper = mapper + (x -> pos)
        pos
    }
}
