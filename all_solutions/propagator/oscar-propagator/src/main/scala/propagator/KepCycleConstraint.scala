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

class KepCycleConstraint(
  val X:          Array[CPIntVar],
  val problem:    ProblemInstance,
  val cycleLimit: Int,
  val optValue:   Int)
  extends Constraint(X(0).store, "KepCycle") {

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 2

  private[this] val arity = X.length
  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)

  private[this] val valueToIndex: Array[HashMap[Int, Int]] =
    Array.tabulate(arity)(i => new HashMap[Int, Int])
  private[this] val nValues: Array[Int] = Array.fill(arity)(0)

  for (i <- 0 until arity) {
    var j = 0
    for (value <- X(i).min until X(i).max + 1)
      if (X(i).hasValue(value)) {
        valueToIndex(i) += (value -> j)
        nValues(i) += 1
        j += 1
      }
  }

  private[this] val unBoundVars = Array.tabulate(arity)(i => i)
  private[this] val unBoundVarsSize = new ReversibleInt(s, arity)

  private[this] val maxDomain = X.maxBy(_.size).size
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = 0

  private[this] val cplex = new IloCplex
  private[this] val cycles = problem.getCycles(cycleLimit)
  private[this] val nCycles = cycles.length
  private[this] val cycleVars = Array.fill[IloNumVar](nCycles)(null)
  private[this] val edgeToCycles = mutable.Map[(Int, Int), ListBuffer[Int]]()

  private[this] val initialMask = {
    val indices = new ArrayBuffer[Int]
    for (cycleIndex <- 0 until nCycles) {
      val cycle = cycles(cycleIndex)
      val cycleEdges = (1 until cycle.length).map(i => (cycle(i - 1), cycle(i))) ++
        List((cycle.last, cycle.head))

      var remove = false
      for ((from, to) <- cycleEdges)
        if (!X(from).hasValue(to))
          remove = true

      if (!remove)
        indices.append(cycleIndex)
    }
    indices
  }

  private[this] val validCycles = new ReversibleSparseBitSet(s, nCycles, initialMask)
  private[this] val currentMask = new validCycles.BitSet(initialMask)

  private[this] val variableValueSupports = Array.tabulate(arity)(i => new Array[validCycles.BitSet](nValues(i)))
  private[this] def getSupports(varIndex: Int, value: Int) = variableValueSupports(varIndex)(valueToIndex(varIndex).getOrElse(value, -1))

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
    var i = 0
    while (i < arity) {
      deltas(i) = X(i).callPropagateOnChangesWithDelta(this)
      i += 1
    }

    computeMasks()
    setupModel()

  }

  @inline private def updateDelta(varIndex: Int, delta: DeltaIntVar): Unit = {

    val intVar = X(varIndex)
    val varSize = intVar.size
    var changed = false

    validCycles.clearCollected()

    /* Use delta to update validTuples */
    domainArraySize = delta.fillArray(domainArray)

    var i = 0
    /* Collect all the removed tuples by doing or's with precomputed masks */
    while (i < domainArraySize) {
      validCycles.collect(getSupports(varIndex, domainArray(i)))
      i += 1
    }

    /* Remove from the valid supports all the collected tuples, no longer supported */
    validCycles.reverseCollected()

    changed = validCycles.intersectCollected()

    /* Failure if there are no more valid tuples */
    if (validCycles.isEmpty())
      throw Inconsistency
  }

  override def propagate(): Unit = {

    var unBoundVarsSize_ = unBoundVarsSize.value
    var j = unBoundVarsSize.value

    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)

      if (deltas(varIndex).size > 0)
        updateDelta(varIndex, deltas(varIndex))

      if (X(varIndex).isBound) {
        unBoundVarsSize_ -= 1
        unBoundVars(j) = unBoundVars(unBoundVarsSize_)
        unBoundVars(unBoundVarsSize_) = varIndex
      }
    }

    unBoundVarsSize.value = unBoundVarsSize_

    solveRelaxedKEP

  }

  def solveRelaxedKEP: Unit = {

    val (toZeros, toOnes) = validCycles.getDiff(currentMask)
    toZeros.map(cycleVars(_).setUB(0))
    toOnes.map(cycleVars(_).setUB(1))

    cplex.solve

    val status = cplex.getStatus
    if (status == IloCplex.Status.Infeasible)
      throw Inconsistency

    if (status == IloCplex.Status.Optimal &&
      cplex.getObjValue + 1e-5 < optValue)
      throw Inconsistency
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def computeMasks(): Unit = {

    val varValueSupports = Array.tabulate(X.length)(i => Array.tabulate(nValues(i))(v => new ArrayBuffer[Int]()))

    for (cycleIndex <- 0 until nCycles) {
      val cycle = cycles(cycleIndex)

      val cycleEdges = (1 until cycle.length).map(i => (cycle(i - 1), cycle(i))) ++
        List((cycle.last, cycle.head))

      for ((from, to) <- cycleEdges)
        if (valueToIndex(from).contains(to))
          varValueSupports(from)(valueToIndex(from).getOrElse(to, -1)) += cycleIndex

    }

    for (varIndex <- 0 until arity)
      for (valueIndex <- 0 until nValues(varIndex))
        variableValueSupports(varIndex)(valueIndex) = new validCycles.BitSet(varValueSupports(varIndex)(valueIndex))

  }
}