package samples

import org.bohao.decaf.types._
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/8/26.
  */
class TypeTest extends FunSpec {
        describe("function type") {
            it("test1") {
                val type1 = new FunctionType(List(IntType, BoolType),
                    new ArrayType(IntType, 10))

                val type2 = new FunctionType(List(IntType, BoolType),
                    new ArrayType(IntType, 10))

                val type3 = new FunctionType(List(IntType, BoolType), VoidType)

                assert(type1 == type2)
                assert(type1 != type3)
            }
        }
}
