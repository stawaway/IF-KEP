package propagator

import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPVar
import oscar.cp.core.CPPropagStrength
import scala.collection.mutable.ArrayBuffer
import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleBoolean
import oscar.cp.core.variables.CPIntVar

class Cover(
  val X: Array[CPIntVar])
  extends Constraint(X(0).store, "KepCover") {

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  override def associatedVars(): Iterable[CPVar] = X

  override def setup(l: CPPropagStrength): Unit = {
    X.map(_.callPropagateWhenDomainChanges(this))
  }

  private[this] var mustCoverIndices: Array[Int] = null
  private[this] var mustCoverSize: Int = 0

  def updateMustCover(mc: Array[Int]): Unit = {
    mustCoverIndices = mc
    mustCoverSize = mc.length
  }

  override def propagate(): Unit = {

    if (mustCoverSize == 0)
      return

    var unCoveredIndex = -1
    var j = -1
    var i = 0
    while (i < mustCoverSize) {
      j = mustCoverIndices(i)

      // individual j is covered
      if (X(j).isBound) {
        if (X(j).max != j)
          return
      } else {
        // individual j can still be covered
        if (unCoveredIndex != -1)
          return
        unCoveredIndex = j
      }

      i += 1
    }

    if (unCoveredIndex == -1)
      throw Inconsistency
    else
      X(unCoveredIndex).removeValue(unCoveredIndex)

  }
}


