package samples

import org.bohao.decaf.x.DomBuilder
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/9/19.
  */
class DomTest extends FunSpec{
    describe("dom") {
        it("1") {
            val builder = new DomBuilder[String]
            builder.addNode("0")
            builder.addNode("1")
            builder.addNode("2")
            builder.addNode("3")
            builder.addNode("4")
            builder.addNode("5")
            builder.addNode("6")
            builder.addNode("7")
            builder.addNode("8")

            builder.addEdge("0", "1")
            builder.addEdge("1", "2")
            builder.addEdge("1", "5")
            builder.addEdge("2", "3")
            builder.addEdge("3", "4")
            builder.addEdge("3", "1")
            builder.addEdge("5", "6")
            builder.addEdge("5", "8")
            builder.addEdge("6", "7")
            builder.addEdge("8", "7")
            builder.addEdge("7", "3")

            builder.buildDom()
            builder.buildIDom()
            builder.buildDf()

            assert(builder.dom("8") == Set("0", "5", "1", "8"))
            assert(builder.idom("1") == "0")
            assert(builder.idom("2") == "1")
            assert(builder.idom("3") == "1")
            assert(builder.idom("4") == "3")
            assert(builder.idom("5") == "1")
            assert(builder.idom("6") == "5")
            assert(builder.idom("7") == "5")
            assert(builder.idom("8") == "5")
            assert(builder.df("1") == Set("1"))
            assert(builder.df("2") == Set("3"))
            assert(builder.df("3") == Set("1"))
            assert(builder.df("5") == Set("3"))
            assert(builder.df("6") == Set("7"))
            assert(builder.df("7") == Set("3"))
            assert(builder.df("8") == Set("7"))
        }

        it("2") {
            val builder = new DomBuilder[String]
            builder.addNode("0")
            builder.addNode("1")
            builder.addNode("2")
            builder.addNode("3")
            builder.addNode("4")
            builder.addNode("5")
            builder.addNode("6")
            builder.addNode("7")
            builder.addNode("8")
            builder.addNode("9")
            builder.addNode("10")
            builder.addNode("11")
            builder.addNode("12")

            builder.addEdge("0", "1")
            builder.addEdge("1", "2")
            builder.addEdge("2", "3")
            builder.addEdge("2", "4")
            builder.addEdge("3", "2")
            builder.addEdge("4", "2")
            builder.addEdge("4", "5")
            builder.addEdge("4", "6")
            builder.addEdge("5", "8")
            builder.addEdge("5", "7")
            builder.addEdge("6", "7")
            builder.addEdge("7", "11")
            builder.addEdge("8", "9")
            builder.addEdge("9", "8")
            builder.addEdge("9", "10")
            builder.addEdge("10", "5")
            builder.addEdge("10", "12")
            builder.addEdge("11", "12")


            builder.buildDom()
            builder.buildIDom()
            assert(builder.dom("4") == Set("0", "1", "2",  "4"))
            assert(builder.idom("1") == "0")
            assert(builder.idom("2") == "1")
            assert(builder.idom("3") == "2")
            assert(builder.idom("4") == "2")
            assert(builder.idom("5") == "4")
            assert(builder.idom("6") == "4")
            assert(builder.idom("7") == "4")
            assert(builder.idom("8") == "5")
            assert(builder.idom("9") == "8")
            assert(builder.idom("10") == "9")
            assert(builder.idom("11") == "7")
            assert(builder.idom("12") == "4")
        }

        it("3") {
            val builder = new DomBuilder[String]
            builder.addNode("0")
            builder.addNode("1")
            builder.addNode("2")
            builder.addNode("3")
            builder.addNode("4")
            builder.addNode("5")
            builder.addNode("6")
            builder.addNode("7")
            builder.addNode("8")
            builder.addNode("9")
            builder.addNode("10")
            builder.addNode("11")
            builder.addNode("12")
            builder.addNode("13")

            builder.addEdge("0", "1")
            builder.addEdge("1", "2")
            builder.addEdge("1", "5")
            builder.addEdge("1", "9")

            builder.addEdge("2", "3")
            builder.addEdge("3", "4")
            builder.addEdge("3", "3")
            builder.addEdge("4", "13")

            builder.addEdge("5", "6")
            builder.addEdge("5", "7")
            builder.addEdge("6", "4")
            builder.addEdge("6", "8")
            builder.addEdge("7", "8")
            builder.addEdge("7", "12")
            builder.addEdge("8", "5")
            builder.addEdge("8", "13")

            builder.addEdge("9", "10")
            builder.addEdge("9", "11")
            builder.addEdge("10", "12")
            builder.addEdge("11", "12")
            builder.addEdge("12", "13")

            builder.buildDom()
            builder.buildIDom()
            builder.buildDf()

            assert(builder.df("5") == Set("4", "5", "12", "13"))
        }
    }
}
