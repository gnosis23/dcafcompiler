package samples

import org.bohao.decaf.types._
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/8/26.
  */
class TypeTest extends FunSpec {
        describe("function type") {
            val type1 = new FunctionType(List(IntType, BoolType),
                new ArrayType(IntType, 10))

            println(type1)
        }
}
