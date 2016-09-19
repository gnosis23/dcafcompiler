package org.bohao.decaf.x

import scala.collection.mutable.ArrayBuffer


/**
  * Created by bohao on 2016/9/19.
  */
class DomBuilder[T] {
    var nodeCount = 0
    private var ids = Map[T, Int]()
    private var values = Map[Int, T]()
    private val nodes = ArrayBuffer[DNode]()
    // Donimator set
    private val Doms = ArrayBuffer[Dom]()
    // Immediate Dominator Point
    private val IDom = ArrayBuffer[Int]()
    // DF
    private val Dfs = ArrayBuffer[DomFrontier]()

    private class DNode {
        var targets : List[Int] = List()
        var prevs : List[Int] = List()

        def addEdge(id : Int): Unit = {
            targets = id :: targets
        }

        def addPrev(id : Int): Unit = {
            prevs = id :: prevs
        }
    }

    private class DomFrontier {
        var targets : Set[Int] = Set()
    }

    private class Dom {
        var set = Set[Int]()
    }

    def addNode(node : T): Unit = {
        ids += (node -> nodeCount)
        values += (nodeCount -> node)

        nodes += new DNode
        Doms += new Dom
        Dfs += new DomFrontier

        IDom += 0
        nodeCount += 1
    }

    def addEdge(from : T, to : T): Unit = {
        val a = ids.get(from)
        val b = ids.get(to)
        (a, b) match {
            case (Some(x), Some(y)) =>
                nodes(x).addEdge(y)
                nodes(y).addPrev(x)
            case _ => throw new Error("bug")
        }
    }

    def buildDom(): Unit = {
        val n = nodeCount
        Doms(0).set += 0

        val all = 0.until(n).toSet
        for (i <- 1 until n) {
            Doms(i).set = all
        }

        var changed = true
        while(changed) {
            changed = false

            for (i <- 1 until n) {
                val prevs = nodes(i).prevs

                var temp = Set[Int]()
                if (prevs.nonEmpty) {
                    val headId = prevs.head
                    temp = Doms(headId).set
                }

                prevs.foreach(j => {
                    temp = temp intersect Doms(j).set
                })

                temp += i

                if (temp != Doms(i).set) {
                    Doms(i).set = temp
                    changed = true
                }
            }
        }
    }

    def buildIDom(): Unit = {
        for (i <- 1 until nodeCount) {
            val size = Doms(i).set.size
            val id = Doms(i).set.find(p => {
                Doms(p).set.size == size - 1
            })
            id match {
                case Some(x) => IDom(i) = x
                case None => new RuntimeException("bug")
            }
        }
    }

    def buildDf(): Unit = {
        for (i <- 1 until nodeCount) {
            val prevs = nodes(i).prevs
            if (prevs.size > 1) {
                prevs.foreach(p => {
                    var runner = p
                    while (runner != IDom(i)) {
                        Dfs(runner).targets += i
                        runner = IDom(runner)
                    }
                })
            }
        }
    }

    def dom(x : T) : Set[T] = {
        val id = ids(x)
        Doms(id).set.map(values)
    }

    def idom(x : T) : T = {
        val id = ids(x)
        values(IDom(id))
    }

    def df(x : T) : Set[T] = {
        val id = ids(x)
        Dfs(id).targets.map(values)
    }
}
