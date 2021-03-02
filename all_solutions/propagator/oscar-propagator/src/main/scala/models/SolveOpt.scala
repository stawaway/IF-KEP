package models

import ilog.cplex.IloCplex
import ilog.concert.IloNumVar
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import ilog.concert.IloNumVarType
import scala.collection.mutable.Queue

class ProblemInstance(fileName: String, verbose: Boolean = false) {
  private[this] val lines: List[String] = Source.fromFile(fileName).getLines().toList
  private[this] val firstLine = lines(0).trim.split("\\s+").toList.map(_.toInt)
  val numVertices = firstLine(0)
  private[this] val graph_ = List.fill(numVertices)(ListBuffer[Int]())

  private[this] val numEdges = firstLine(1)
  for (i <- 0 until numEdges) {
    val edgeLine = lines(i + 1).trim.split("\\s+").toList.map(_.toInt)
    graph_(edgeLine(0)) += edgeLine(1)
  }

  val graph = graph_.map(_.toArray).toArray
  val neighbors = graph_.map(_.toSet).toArray

  val numAltruistic = lines(numEdges + 1).toInt
  val altruisticNodes_ = new ListBuffer[Int]()
  for (i <- 0 until numAltruistic)
    altruisticNodes_ += lines(numVertices + 1 + i).toInt
  private[this] val altruisticNodes = altruisticNodes_.toArray

  def print() = {
    println("number of vertices: " + numVertices)
    println("graph:")
    for (i <- 0 until numVertices)
      println(graph(i).deep.mkString(" "))
    println("number of altruistic nodes: " + numAltruistic)
    println("altruistic nodes:")
    println(altruisticNodes.deep.mkString(" "))
  }

  private[this] var simplePaths: Array[Array[Int]] = null
  private[this] var pathLimit = -1

  def getSimplePaths(pathLimit: Int): Array[Array[Int]] = {
    if (simplePaths == null || this.pathLimit != pathLimit) {
      val simplePaths_ = ListBuffer[List[Int]]()
      val pathQueue = Queue[List[Int]]()

      for (i <- 0 until numVertices)
        pathQueue.enqueue(List(i))

      while (!pathQueue.isEmpty) {
        val path = pathQueue.dequeue
        val last = path.last
        if (path.size < pathLimit) {
          for (u <- graph(last))
            if (!path.contains(u)) {
              val nextPath = path :+ u;
              pathQueue.enqueue(nextPath);
            }
        } else
          simplePaths_ += path
      }
      simplePaths = simplePaths_.map(_.toArray).toArray
      this.pathLimit = pathLimit
    }
    simplePaths
  }

  private[this] var cycles: Array[Array[Int]] = null
  private[this] var cycleLimit = -1

  def getCycles(cycleLimit: Int): Array[Array[Int]] = {
    if (cycles == null || this.cycleLimit != cycleLimit) {
      if (verbose)
        println("Starting to generate the cycles")
      val startTime = System.nanoTime

      val cycles_ = ListBuffer[List[Int]]()
      val pathQueue = Queue[List[Int]]()

      for (i <- 0 until numVertices)
        pathQueue.enqueue(List(i))

      while (!pathQueue.isEmpty) {
        val path = pathQueue.dequeue
        val first = path.head
        val last = path.last

        if (neighbors(last).contains(first))
          cycles_ += path

        if (path.size < cycleLimit)
          for (u <- graph(last))
            if (u > first && !path.contains(u))
              pathQueue.enqueue(path :+ u)
      }
      cycles = cycles_.map(_.toArray).toArray
      this.cycleLimit = cycleLimit

      val runtime = System.nanoTime - startTime
      if (verbose)
        println("Generating the cycles took " + runtime / 1e9 + " seconds")
    }
    cycles
  }
}

class KepEdgeSolver(problem: ProblemInstance, cycleLimit: Int) {
  def solveKEP: Int = {
    val incoming = List.fill(problem.numVertices)(ListBuffer[Int]())
    for (i <- 0 until problem.numVertices)
      for (j <- problem.graph(i))
        incoming(j) += i

    val cplex = new IloCplex
    cplex.setParam(IloCplex.Param.ParamDisplay, false)
    cplex.setParam(IloCplex.Param.MIP.Display, 0)
    cplex.setParam(IloCplex.Param.Simplex.Display, 0)

    val variableMap = mutable.Map[(Int, Int), IloNumVar]()
    val objExpr = cplex.linearNumExpr()
    for (i <- 0 until problem.numVertices)
      for (j <- problem.graph(i)) {
        val x = cplex.numVar(0.0, 1.0, IloNumVarType.Bool)
        variableMap += ((i, j) -> x)
        objExpr.addTerm(1, x)
      }
    cplex.addMaximize(objExpr)

    for (i <- 0 until problem.numVertices) {
      val outgoingExpr = cplex.linearNumExpr()
      for (j <- problem.graph(i))
        outgoingExpr.addTerm(1.0, variableMap((i, j)))

      cplex.addLe(outgoingExpr, 1);

      val incomingExpr = cplex.linearNumExpr()
      for (j <- incoming(i))
        incomingExpr.addTerm(1.0, variableMap((j, i)));

      cplex.addEq(incomingExpr, outgoingExpr);
    }

    val pathLimit = cycleLimit + 1;
    val paths = problem.getSimplePaths(pathLimit);
    for (path <- paths) {
      val expr = cplex.linearNumExpr();
      for (i <- 0 until pathLimit - 1)
        expr.addTerm(1.0, variableMap(path(i), path(i + 1)))
      cplex.addLe(expr, cycleLimit - 1)
    }

    cplex.solve
    cplex.getObjValue.toInt
  }
}

class KepCycleSolver(problem: ProblemInstance, cycleLimit: Int) {
  def solveKEP: Int = {

    val cplex = new IloCplex
    cplex.setParam(IloCplex.Param.ParamDisplay, false)
    cplex.setParam(IloCplex.Param.MIP.Display, 0)
    cplex.setParam(IloCplex.Param.Simplex.Display, 0)

    val cycles = problem.getCycles(cycleLimit)
    val cycleVars = Array.fill[IloNumVar](cycles.length)(null)
    val includedIn = Array.fill(problem.numVertices)(ListBuffer[Int]())
    for (c <- 0 until cycles.length) {
      val cycle = cycles(c)
      for (i <- cycle)
        includedIn(i) += c
    }

    val objExpr = cplex.linearNumExpr()
    for (i <- 0 until cycles.length) {
      val x = cplex.numVar(0.0, 1.0, IloNumVarType.Bool)
      cycleVars(i) = x
      objExpr.addTerm(cycles(i).length, x)
    }
    cplex.addMaximize(objExpr)

    for (i <- 0 until problem.numVertices) {
      val includedExpr = cplex.linearNumExpr()
      for (j <- includedIn(i))
        includedExpr.addTerm(1.0, cycleVars(j))

      cplex.addLe(includedExpr, 1);
    }

    cplex.solve
    cplex.getObjValue.toInt
  }
}


