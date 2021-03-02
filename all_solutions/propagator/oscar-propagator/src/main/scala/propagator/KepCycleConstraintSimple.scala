package propagator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import ilog.concert.IloNumVar
import ilog.concert.IloNumVarType
import ilog.cplex.IloCplex
import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import models.ProblemInstance

class KepCycleConstraintSimple(
  val X:          Array[CPIntVar],
  val problem:    ProblemInstance,
  val cycleLimit: Int,
  val optValue:   Int)
  extends Constraint(X(0).store, "KepCycle") {

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 2

  private[this] val cplex = new IloCplex
  private[this] val cycles = problem.getCycles(cycleLimit)
  private[this] val nCycles = cycles.length
  private[this] val cycleVars = Array.fill[IloNumVar](nCycles)(null)
  private[this] val edgeToCycles = mutable.Map[(Int, Int), ListBuffer[Int]]()

  var loopingTime: Double = 0.0
  var lpSolvingTime: Double = 0.0

  def setupModel(): Unit = {

    for (i <- 0 until problem.numVertices)
      for (j <- problem.graph(i))
        edgeToCycles += ((i, j) -> ListBuffer())

    def addEdge(u1: Int, u2: Int, cid: Int): Unit = edgeToCycles.get((u1, u2)) match {
      case Some(l) => l += cid
      case None    => System.exit(1)
    }

    for (cid <- 0 until cycles.length) {
      val cycle = cycles(cid)
      val l = cycle.length
      for (i <- 1 until l)
        addEdge(cycle(i - 1), cycle(i), cid)
      addEdge(cycle(l - 1), cycle(0), cid)
    }

    cplex.setParam(IloCplex.Param.ParamDisplay, false)
    cplex.setParam(IloCplex.Param.MIP.Display, 0)
    cplex.setParam(IloCplex.Param.Simplex.Display, 0)

    val includedIn = Array.fill(problem.numVertices)(ListBuffer[Int]())
    for (c <- 0 until cycles.length) {
      val cycle = cycles(c)
      for (i <- cycle)
        includedIn(i) += c
    }

    val objExpr = cplex.linearNumExpr()
    for (i <- 0 until cycles.length) {
      val x = cplex.numVar(0.0, 1.0, IloNumVarType.Float)
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

  }

  override def associatedVars(): Iterable[CPVar] = X

  override def setup(l: CPPropagStrength): Unit = {
    X.map(_.callPropagateWhenDomainChanges(this))

    setupModel()

  }

  override def propagate(): Unit = {

    solveRelaxedKEP

  }

  def solveRelaxedKEP: Unit = {

    val loopingStartTime = System.nanoTime

    val removedCycleIds = mutable.Set[Int]()
    for (i <- 0 until problem.numVertices)
      for (j <- problem.graph(i))
        if (!X(i).hasValue(j))
          removedCycleIds ++= edgeToCycles(i, j)

    cycleVars.foreach(i => { i.setLB(0); i.setUB(1) })
    removedCycleIds.map(i => cycleVars(i).setUB(0))

    loopingTime += (System.nanoTime - loopingStartTime) / 1e9

    val solvingStartTime = System.nanoTime
    cplex.solve
    lpSolvingTime += (System.nanoTime - solvingStartTime) / 1e9

    val status = cplex.getStatus
    if (status == IloCplex.Status.Infeasible)
      throw Inconsistency

    if (status == IloCplex.Status.Optimal &&
      cplex.getObjValue + 1e-5 < optValue)
      throw Inconsistency
  }

}