package models

import java.io.File
import oscar.cp.`package`.CPModel
import scala.io.Source
import oscar.cp.core.variables.CPIntVar
import oscar.cp.constraints.EqReif
import oscar.cp.core.variables.CPBoolVar
import oscar.cp._
import oscar.cp.constraints.Or
import propagator.KepCycleConstraint
import propagator.KepEdgeConstraint
import java.io.PrintWriter
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import oscar.algo.Inconsistency
import oscar.algo.search.DFSearch
import propagator.Cover
import propagator.KepCover
import propagator.KepCover

object GreedyKep extends CPModel with App {
  val parser = argsParser()
  parser.parse(args, Config()) match {
    case Some(config) =>
      val startTime = System.nanoTime
      val maxTime = (config.timeLimit * 1000) + System.currentTimeMillis()

      val problem = new ProblemInstance(config.kepFile, config.verbose)
      val cycleLimit = config.cycleLimit
      val cycleSolver = new KepCycleSolver(problem, cycleLimit)
      val optValue = cycleSolver.solveKEP

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

      add(sum(selfLoop) === problem.numVertices - optValue)

      val coverConstraint = new Cover(successor)
      add(coverConstraint)

      var kepCoverConstraint: KepCover = null
      if (config.lpPropagator) {
        kepCoverConstraint = new KepCover(successor, problem, cycleLimit, optValue)
        add(kepCoverConstraint)
      }

      var pw: PrintWriter = null
      var outputFile: File = null
      var printSolutions = (config.outputFile != "")

      if (printSolutions) {
        outputFile = new File(config.outputFile)
        pw = new PrintWriter(new FileOutputStream(outputFile, true))
      }

      val notCovered = (0 until problem.numVertices).to[scala.collection.mutable.Set]
      var allCovered = false

      search {
        config.branching match {
          case "first-fail"        => binaryFirstFail(successor)
          case "conflict-ordering" => conflictOrderingSearch(successor, i => (successor(i).size, i), successor(_).min)
        }
      } onSolution {
        val row = successor.zipWithIndex.filter(i => i._1.value != i._2).map(_._2)
        if (printSolutions)
          pw.append(row.mkString(" ") + "\n")

        notCovered --= row.toSet
        if (notCovered.isEmpty)
          allCovered = true
        else {
          coverConstraint.updateMustCover(notCovered.toArray)
          if (config.lpPropagator)
            kepCoverConstraint.updateMustCover(notCovered.toArray)

        }

      }

      val checkTime = config.timeLimit < Int.MaxValue
      val stats = start {
        var stop = false
        stop |= (checkTime && System.currentTimeMillis() >= maxTime)
        stop |= allCovered
        stop
      }

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
    new scopt.OptionParser[Config]("GreedyKEP") {
      head("GreedyKep", "1.0")

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

      opt[String]("output-file") optional () valueName ("<file>") action { (x, c) =>
        c.copy(outputFile = x)
      } validate { x =>
        val y = new File(x)
        if (y.exists()) {
          y.delete()
          success
        } else if (y.createNewFile())
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

