package mdd

import models.ProblemInstance
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.Queue
import java.io.PrintWriter
import java.io.FileOutputStream
import java.io.File

case class MddNode(val state: immutable.Set[Int]) {
  var id = -1
  var variable: Int = -1
  var longestPath: Int = 0
  var hi: MddNode = null
  var lo: MddNode = null
  var parents = ArrayBuffer[MddNode]()

  override def hashCode: Int = state.hashCode
  override def equals(that: Any): Boolean =
    that match {
      case that: MddNode => {
        this.state == that.state
      }
      case _ => false
    }
}

object RelaxedMdd extends App {

  val startTime = System.nanoTime

  val vertices = 70
  val instance_id = 18

  val instance = new ProblemInstance("../../data/porto/p." + vertices + "." + instance_id + ".txt")
  val cycles = instance.getCycles(3)
  val nCycles = cycles.length

  val remainingVars = mutable.Set(0 until nCycles: _*)
  val vertexToCycles = Array.fill(instance.numVertices)(ArrayBuffer[Int]())
  for ((cycle, c) <- cycles.zipWithIndex)
    for (vertex <- cycle)
      vertexToCycles(vertex) += c

  def selectVar(layer: Array[MddNode]): Int = {
    val cycleCount = Array.fill(nCycles)(0)
    for (node <- layer)
      for (c <- node.state)
        cycleCount(c) += 1

    val minIndex = cycleCount.zipWithIndex.filter(i => remainingVars.contains(i._2)).minBy(_._1)._2
    remainingVars -= minIndex

    minIndex
  }

  def createChild(parent: MddNode, variable: Int, value: Int): MddNode = {
    val childState = value match {
      case 0 => parent.state - variable
      case 1 => parent.state -- cycles(variable).map(vertexToCycles(_).toSet).reduce(_ ++ _)
    }
    MddNode(childState)
  }

  def mergeNodes(nodes: ArrayBuffer[MddNode]): MddNode = {
    val state = nodes.map(_.state).reduce(_ union _)
    val node = MddNode(state)
    node.id = getId

    node.parents ++= nodes.map(_.parents.toSet).reduce(_ union _)
    node.longestPath = nodes.map(_.longestPath).reduce(_ max _)

    for (n <- nodes)
      for (p <- n.parents) {
        if (p.hi == n)
          p.hi = node
        if (p.lo == n)
          p.lo = node
      }

    node
  }

  def shrinkLayer(layer: ArrayBuffer[MddNode], width: Int): Array[MddNode] = {
    if (layer.length > width) {
      val sortedLayer = layer.sortBy(_.longestPath)
      val i = layer.length - width
      val mergedNode = mergeNodes(sortedLayer.take(i + 1))

      (sortedLayer.takeRight(width - 1) :+ mergedNode).toArray
    } else
      layer.toArray
  }

  var idCounter = -1
  def getId: Int = { idCounter += 1; idCounter }

  val root = new MddNode((0 until cycles.length).toSet)
  root.id = getId
  var currentLayer = Array(root)

  while (!remainingVars.isEmpty) {
    val variable = selectVar(currentLayer)
    val nextLayer = ArrayBuffer[MddNode]()
    val nextLayerNodes = HashMap[Set[Int], MddNode]()

    def addNode(node: MddNode): MddNode = {
      nextLayerNodes.get(node.state) match {
        case Some(n) => n
        case None => {
          node.id = getId
          nextLayerNodes.put(node.state, node)
          nextLayer += node
          node
        }
      }
    }

    for (node <- currentLayer)
      for (childValue <- 0 to 1)
        if (childValue == 0 || node.state.contains(variable)) {

          val childNode_ = createChild(node, variable, childValue)
          val childNode = addNode(childNode_)

          val ch = childValue match {
            case 0 => node.lo = childNode
            case 1 => node.hi = childNode
          }

          childNode.parents += node
          val childPathLength = node.longestPath + childValue
          if (childNode.longestPath < childPathLength)
            childNode.longestPath = childPathLength
        }

    currentLayer = shrinkLayer(nextLayer, remainingVars.size)
  }

  def printMdd(root: MddNode) {

    val nodeIds = ArrayBuffer[Int]()
    val edges = ArrayBuffer[Tuple3[Int, Int, Int]]()

    val Q = Queue[MddNode]()
    val S = mutable.Set[Int]()

    Q.enqueue(root)
    S += root.id

    while (!Q.isEmpty) {
      val node = Q.dequeue
      nodeIds += node.id

      val hiChild = node.hi
      if (hiChild != null) {
        edges += Tuple3(node.id, hiChild.id, 1)
        if (!S.contains(hiChild.id)) {
          Q.enqueue(hiChild)
          S += hiChild.id
        }
      }

      val loChild = node.lo
      if (loChild != null) {
        edges += Tuple3(node.id, loChild.id, 0)
        if (!S.contains(loChild.id)) {
          Q.enqueue(loChild)
          S += loChild.id
        }
      }

    }

    println(nodeIds.size)
    //    val dotStr = new StringBuilder()
    //    dotStr ++= "digraph mdd {\n"
    //    for (i <- nodeIds)
    //      dotStr ++= "node" + i + "[label=\"n" + i + "\"];\n"
    //    for ((pid, cid, v) <- edges) {
    //      val style = if (v == 0) "dashed" else "bold"
    //      dotStr ++= "node" + pid + " -> node" + cid + " [style=\"" + style + "\"];\n"
    //    }
    //    dotStr ++= "}\n"
    //
    //    val pw = new PrintWriter(new FileOutputStream(new File("viz/test.dot"), false))
    //    pw.append(dotStr)
    //    pw.close

  }

  printMdd(root)
  println((System.nanoTime - startTime) / 1e9)

}