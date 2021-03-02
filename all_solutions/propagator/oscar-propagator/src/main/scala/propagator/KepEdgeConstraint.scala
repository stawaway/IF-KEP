package propagator

import models.ProblemInstance
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPVar
import oscar.cp.core.CPPropagStrength
import scala.collection.mutable.ListBuffer
import ilog.cplex.IloCplex
import ilog.concert.IloNumVar
import scala.collection.mutable
import ilog.concert.IloNumVarType
import oscar.algo.Inconsistency

class KepEdgeConstraint(
  val X:          Array[CPIntVar],
  val problem:    ProblemInstance,
  val cycleLimit: Int,
  val optValue:   Int)
  extends Constraint(X(0).store, "KepEdge") {
  
  idempotent = true

  var loopingTime: Double = 0.0
  var lpSolvingTime: Double = 0.0

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
      val x = cplex.numVar(0.0, 1.0, IloNumVarType.Float)
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

  override def associatedVars(): Iterable[CPVar] = X

  override def setup(l: CPPropagStrength): Unit = {
    X.map(_.callPropagateWhenDomainChanges(this))
    propagate()
  }

  override def propagate(): Unit = {
    solveRelaxedKEP
  }

  def solveRelaxedKEP: Unit = {

    val loopingStartTime = System.nanoTime

    variableMap.map(_._2).foreach(i => { i.setLB(0); i.setUB(1) })

    for (i <- 0 until problem.numVertices)
      if (X(i).isBound && X(i).value != i)
        variableMap(i, X(i).value).setLB(1)
      else {
        for (j <- problem.graph(i))
          if (!X(i).hasValue(j))
            variableMap(i, j).setUB(0)
      }

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