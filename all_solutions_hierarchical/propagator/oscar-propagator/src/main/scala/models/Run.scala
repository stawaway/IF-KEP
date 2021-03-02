package models

import java.io.File
import oscar.cp.`package`.CPModel
import scala.io.Source
import oscar.cp.core.variables.CPIntVar
import oscar.cp.constraints.EqReif
import oscar.cp.core.variables.CPBoolVar
import oscar.cp._
import oscar.cp.constraints.Or
import oscar.cp.constraints.And
import propagator.KepCycleConstraint
import propagator.KepEdgeConstraint
import java.io.PrintWriter
import java.io.FileOutputStream

case class Config(
  kepFile:        String  = "",
  outputFile:     String  = "",
  cycleLimit:     Int     = 3,
  timeLimit:      Int     = Int.MaxValue,
  branching:      String  = "first-fail",
  lpPropagator:   Boolean = false,
  edgePropagator: Boolean = false,
  verbose:        Boolean = false)

object EnumKep extends CPModel with App {
  val parser = argsParser()
  parser.parse(args, Config()) match {
    case Some(config) =>
      val startTime = System.nanoTime

      val problem = new ProblemInstance(config.kepFile, config.verbose)
      val cycleLimit = config.cycleLimit
      val cycleSolver = new KepHierarchicalSolver(problem, cycleLimit)
      val (cycles, backArcs, numPatients, numCycles, numBackArcs) = cycleSolver.solveKEP

      val successor = Array.tabulate(problem.numVertices)(
        i => CPIntVar(problem.graph(i).+:(i)))

      add(allDifferent(successor))

      val selfLoop = Array.fill(problem.numVertices)(CPBoolVar())

      for (i <- 0 until problem.numVertices) {
        add(new EqReif(successor(i), i, selfLoop(i)))

        val b2 = CPBoolVar()
        add(new EqReif(successor(successor((i))), i, b2))

        val b3 = CPBoolVar()
        add(new EqReif(successor(successor(successor((i)))), i, b3))

        add(new Or(Array(selfLoop(i), b2, b3)))
      }

      add(sum(selfLoop) === problem.numVertices - numPatients)

      val indicators: Array[CPBoolVar] = new Array[CPBoolVar](cycles.length)

      for (i <- 0 until cycles.length) {
        val cycle = cycles(i)
        val I_c = CPBoolVar()

        val b = Array.fill(cycle.length)(CPBoolVar())

        for (j <- 0 until cycle.length) {
          val j_ = (j+1) % cycle.length
          add(new EqReif(successor(cycle(j)), cycle(j_), b(j)))
        }

        add(new And(b, I_c))
        indicators(i) = I_c
      }

      add(sum(Array.tabulate(cycles.length)(i => indicators(i))) === numCycles)
      add(sum(Array.tabulate(cycles.length)(i => indicators(i) * backArcs(i))) === numBackArcs)

      if (config.lpPropagator) {
        val C = new KepCycleConstraint(successor, problem, cycleLimit, numPatients)
        add(C)
      }

      if (config.edgePropagator) {
        val C = new KepEdgeConstraint(successor, problem, cycleLimit, numPatients)
        add(C)
      }

      var pw: PrintWriter = null
      var outputFile: File = null
      var printSolutions = (config.outputFile != "")

      if (printSolutions) {
        outputFile = new File(config.outputFile)
        pw = new PrintWriter(new FileOutputStream(outputFile, true))
      }

      search {
        config.branching match {
          case "first-fail"        => binaryFirstFail(successor)
          case "conflict-ordering" => conflictOrderingSearch(successor, i => (successor(i).size, i), successor(_).min)
        }
      } onSolution {
        if (printSolutions) {
          val row = successor.zipWithIndex.filter(i => i._1.value != i._2).map(_._2)
          pw.append(row.mkString(" ") + "\n")
        }
      }

      val stats = start(timeLimit = config.timeLimit)
      if (printSolutions)
        pw.close()

      if (stats.completed) {
        println("failures:" + stats.nFails)
        println("nodes:" + stats.nNodes)
        println("solutions:" + stats.nSols)
        println("solving time:" + stats.time / 1e3)

        val totalTime = System.nanoTime - startTime
        println("total time:" + totalTime / 1e9)
      } else {
        println("The enumeration was not completed within the time limit")
        if (printSolutions)
          outputFile.delete()
      }

    case None =>
  }

  def argsParser(): scopt.OptionParser[Config] = {
    new scopt.OptionParser[Config]("KepEnumerator") {
      head("KepEnumerator", "1.0")

      opt[String]("kep-file") required () valueName ("<file>") action { (x, c) => c.copy(kepFile = x)
      } validate { x =>
        {
          val y = new File(x)
          if (y.exists()) success else failure("<KEP File> does not exist")
        }
      } text ("the input KEP file")

      opt[String]("branching") optional () valueName ("<strategy>") action { (x, c) =>
        c.copy(branching = x)
      } validate { x =>
        if (List("first-fail", "conflict-ordering") contains x) success else failure("unknown <strategy>")
      } text ("variable/value selection strategy, default: first-fail")

      opt[Int]("cycle-limit") optional () valueName ("<limit>") action { (x, c) =>
        c.copy(cycleLimit = x)
      } validate { x => if (x >= 2) success else failure("<cycle limit> should be at least 2")
      } text ("the limit on cycle length, default: 3")

      opt[Int]("time-limit") optional () valueName ("<limit>") action { (x, c) =>
        c.copy(timeLimit = x)
      } validate { x => if (x > 0) success else failure("<time limit> should be positive")
      } text ("time limit in seconds")

      opt[Unit]("LP-prop") action { (_, c) =>
        c.copy(lpPropagator = true)
      } text ("use the LP-based propagator")

      opt[Unit]("edge-prop") action { (_, c) =>
        c.copy(edgePropagator = true)
      } text ("use the edge-based propagator")

      opt[String]("output-file") optional () valueName ("<file>") action { (x, c) =>
        c.copy(outputFile = x)
      } validate { x =>
        val y = new File(x)
        if (y.exists())
          failure("the output file already exists")
        else if (y.createNewFile())
          success
        else
          failure("can not create the output file")
      } text ("write the list of all solutions to <file>")

      opt[Unit]("verbose") abbr ("v") action { (_, c) =>
        c.copy(verbose = true)
      } text ("output more details")

      help("help") text ("Usage of Tester")

      override def showUsageOnError = Some(true)
    }
  }
}

