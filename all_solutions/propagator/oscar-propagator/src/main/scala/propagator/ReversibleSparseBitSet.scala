package propagator

import oscar.algo.reversible.ReversibleLong
import oscar.algo.reversible.TrailEntry
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.TrailEntry
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.BitSetOp._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

/* Trailable entry to restore the value of the ith Long of the valid tuples */
final class ReversibleSparseBitSetEntry(set: ReversibleSparseBitSet, numberOfValues: Int) extends TrailEntry {
  @inline override def restore(): Unit = set.restore(numberOfValues)
}

/**
 * A reversible set with an internal bit-set representation.
 * This set can remove efficiently its elements from another bit-set
 * This set can compute efficiently its intersection with another bit-set
 * @param context reversible context used for trailing
 * @param n initial values must be taken from {0,...,n-1}
 * @param initialValues the initial values contained in the set
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleSparseBitSet(val context: ReversibleContext, val n: Int, val initialValues: Iterable[Int]) {

  /**
   * Immutable bit-set that can be used to remove/intersect
   * with the the ReversibleSparseBitSet
   * @param values initial values, they must be in {0,...,n-1}
   */
  class BitSet(values: Iterable[Int]) {
    assert(values.forall(v => v < n && v >= 0), "Trying to put a value out of the bound of the BitSet")

    /* Variable used to store value of the bitset */
    protected[ReversibleSparseBitSet] var words: Array[Long] = Array.fill(nWords)(0L)

    /* Variable used to keep track of the last word intersecting with this */
    protected[ReversibleSparseBitSet] var lastSupport = 0

    private[this] val mask: Long = ~0L >>> (64 - (n % 64))

    /* Put initial values */
    values.foreach(v => setBit(words, v))

    /**
     * Put value of given bit b to 1
     * @param b id of bit
     */
    def set(b: Int): Unit = {
      assert(b < n && b >= 0, "Trying to put a value out of the bound of the BitSet")
      setBit(words, b)
    }

    /**
     * Check if this is empty
     * @return true if this is empty
     */
    def isEmpty: Boolean = {
      words.forall(_ == 0)
    }

    /**
     * Compute intersection between this and bs
     * This contains the result
     * @param bs bitset to intersect
     */
    def &=(bs: BitSet): Unit = {
      var i = words.length
      while (i > 0) {
        i -= 1
        words(i) = words(i) & bs.words(i)
      }
    }

    /**
     * Compute intersection between this and logical negation of bs
     * This contains the result
     * @param bs bitset to intersect
     */
    def &~=(bs: BitSet): Unit = {
      var i = words.length
      while (i > 0) {
        i -= 1
        words(i) = words(i) & ~bs.words(i)
      }
    }

    /**
     * Compute union between this and bs
     * This contains the result
     * @param bs bitset to unite
     */
    def |=(bs: BitSet): Unit = {
      var i = words.length
      while (i > 0) {
        i -= 1
        words(i) = words(i) | bs.words(i)
      }
    }

    /**
     * Compute logical negation of this
     * This contains the result
     */
    def unary_~ : Unit = {
      var i = words.length
      while (i > 0) {
        i -= 1
        words(i) = ~words(i)
      }
      words(words.length - 1) = words(words.length - 1) & mask
    }

    override def toString: String = {
      val size = n min 64
      words.map(e => String.format(s"%${size}s", java.lang.Long.toBinaryString(e)).replace(' ', '0')).mkString(" ")
    }
  }

  assert(initialValues.forall(v => v < n && v >= 0), "Trying to put a value out of the bound of the ReversibleSparseBitSet")

  /* Variables used to store value of the bitset */
  private[this] val nWords = bitLength(n)
  private[this] val words: Array[Long] = Array.fill(nWords)(0L)

  /* Variables used to make set sparse */
  private[this] val nonZeroIdx: Array[Int] = Array.tabulate(nWords)(i => i)
  private[this] var nNonZero: Int = nWords

  /* Variable used to make computation */
  private[this] val tempMask = Array.fill(nWords)(0L)

  /* Variables used for the trailing */
  private[this] var timeStamp = -1L
  private[this] var innerTrailSize = 1000
  private[this] var nTrailEntries = 0
  private[this] var wordIndex = Array.ofDim[Int](innerTrailSize)
  private[this] var wordValue = Array.ofDim[Long](innerTrailSize)

  /* Put initial values and find initial nonEmpty words */
  initialValues.foreach(v => setBit(words, v))
  var i: Int = nNonZero
  while (i > 0) {
    i -= 1
    if (words(nonZeroIdx(i)) == 0L) {
      nNonZero -= 1
      nonZeroIdx(i) = nonZeroIdx(nNonZero)
      nonZeroIdx(nNonZero) = i
    }
  }

  /**
   * Grow trailing array variables
   */
  @inline private[this] def growInnerTrail(): Unit = {
    val newWordIndex = new Array[Int](innerTrailSize * 2)
    val newWordValue = new Array[Long](innerTrailSize * 2)
    System.arraycopy(wordIndex, 0, newWordIndex, 0, innerTrailSize)
    System.arraycopy(wordValue, 0, newWordValue, 0, innerTrailSize)
    wordIndex = newWordIndex
    wordValue = newWordValue
    innerTrailSize *= 2
  }

  /**
   * Restore structure to a previous state
   */
  @inline final def restore(numberOfValues: Int): Unit = {
    var k = numberOfValues
    while (k > 0) {
      val pos = nTrailEntries - k
      words(wordIndex(pos)) = wordValue(pos)
      k -= 1
    }
    nTrailEntries -= numberOfValues
    nNonZero = numberOfValues
  }

  /**
   * Save current state of structure
   */
  private[this] def trail(): Unit = {
    while (nTrailEntries + nNonZero > innerTrailSize) growInnerTrail()
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      val word = words(offset)
      wordIndex(nTrailEntries) = offset
      wordValue(nTrailEntries) = word
      nTrailEntries += 1
    }
    val trailEntry = new ReversibleSparseBitSetEntry(this, nNonZero)
    context.trail(trailEntry)
  }

  /**
   * Check if reversible sparse bit set is empty
   * @return true if this is empty, false otherwise
   */
  def isEmpty(): Boolean = {
    nNonZero == 0
  }

  /**
   * Check if there is at most two non-empty words
   * @return true if this has <=2 non-empty words, false otherwise
   */
  def isDuo(): Boolean = {
    nNonZero <= 2
  }

  /**
   * Clear the collected elements set
   */
  def clearCollected(): Unit = {
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      tempMask(nonZeroIdx(i)) = 0L
    }
  }

  /**
   * Compute union between set of already
   * collected elements and bs
   * Union is stored internally
   * @param bs bitset to add
   */
  def collect(bs: BitSet): Unit = {
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      tempMask(offset) |= bs.words(offset)
    }
  }

  /**
   * Compute intersection between set of already
   * collected elements and bs
   * Intersection is stored internally
   * @param bs bitset to intersect
   */
  def collectByIntersection(bs: BitSet): Unit = {
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      tempMask(offset) &= bs.words(offset)
    }
  }

  /**
   * Compute logical negation of the set of
   * already collected elements
   * Negation is stored internally
   */
  def reverseCollected(): Unit = {
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      tempMask(offset) = ~tempMask(offset)
    }
  }

  /**
   * Compute intersection between set of
   * collected elements and this
   * This contains the intersection
   * @return true if this has changed, false otherwise
   */
  def intersectCollected(): Boolean = {
    if (context.magic != timeStamp) {
      trail()
      timeStamp = context.magic
    }

    var changed = false
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      val oldLong: Long = words(offset)
      val newLong: Long = oldLong & tempMask(offset)
      words(offset) = newLong
      /* Remove the word from the sparse set if equal to 0 */
      if (newLong == 0L) {
        nNonZero -= 1
        nonZeroIdx(i) = nonZeroIdx(nNonZero)
        nonZeroIdx(nNonZero) = offset
      }
      changed |= oldLong != newLong
    }
    changed
  }

  /**
   * Compute intersection between logical negation of set of
   * collected elements and this
   * This contains the intersection
   * @return true if this has changed, false otherwise
   */
  def removeCollected(): Boolean = {
    reverseCollected()
    val b = intersectCollected()
    reverseCollected()
    b
  }

  /**
   * Check if there is a non-empty intersection between this and bs
   * @param bs bitset to intersect
   * @return true if intersection is non-empty
   */
  def intersect(bs: BitSet): Boolean = {
    val support = bs.lastSupport

    if ((words(support) & bs.words(support)) != 0L) {
      return true
    }

    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if ((words(offset) & bs.words(offset)) != 0L) {
        /* We found a support and we store the index of the Long where the support is */
        bs.lastSupport = offset
        return true
      }
    }

    false
  }

  /**
   * Count the number of elements in intersection
   * between this and bs
   * @param bs bitset to intersect
   * @return number of elements in intersection
   */
  def intersectCount(bs: BitSet): Int = {
    var count = 0

    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      count += java.lang.Long.bitCount(words(offset) & bs.words(offset))
    }

    count
  }

  /**
   * Count the number of elements in intersection
   * between this, bs1 and bs2
   * @param bs1, bs2 bitsets to intersect
   * @return number of elements in intersection
   */
  def intersectCount(bs1: BitSet, bs2: BitSet): Int = {
    var count = 0

    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      count += java.lang.Long.bitCount(words(offset) & bs1.words(offset) & bs2.words(offset))
    }

    count
  }

  /**
   * Compute weighted count of intersection
   * between this and bs
   * @param bs bitset to intersect
   * @param offsetToWeightId array to map word id to corresponding weight id
   * @param weightFct array to map weight id to the corresponding weight function
   * @return weighted count of intersection
   */
  def intersectCount(bs: BitSet, offsetToWeightId: Array[Int], weightFct: Array[() => Int]): Int = {
    var count = 0

    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      count += java.lang.Long.bitCount(words(offset) & bs.words(offset)) * weightFct(offsetToWeightId(offset))()
    }

    count
  }

  override def toString(): String = {
    val size = n min 64
    def format(l: Long) = String.format(s"%${size}s", java.lang.Long.toBinaryString(l)).replace(' ', '0')
    "NonZeroWords:" + nNonZero + " words:" + words.map(format(_)).mkString(" , ")
  }

  def getBit(b: Int): Boolean = {
    (words(bitOffset(b)) & oneBitLong(b)) != 0L
  }

  def getOnes(bitset_ :Long, offset: Int, ones: ArrayBuffer[Int]): Unit = {
    var bitset = bitset_
    while (bitset != 0) {
      val t = bitset & -bitset
      val r = java.lang.Long.numberOfTrailingZeros(bitset)
      ones.append(offset * 64 + r)
      bitset ^= t
    }
  }

  def getDiff(current: BitSet): (ArrayBuffer[Int], ArrayBuffer[Int]) = {
    val toZeros = new ArrayBuffer[Int]
    val toOnes = new ArrayBuffer[Int]

    var i: Int = 0
    while (i < words.length) {
      if (current.words(i) != 0 || words(i) != 0) {
        val toOneBits = ~current.words(i) & words(i)
        getOnes(toOneBits, i, toOnes)

        val toZeroBits = current.words(i) & ~words(i)
        getOnes(toZeroBits, i, toZeros)

        current.words(i) = words(i)
      }
      i += 1
    }
    (toZeros, toOnes)
  }

  def printTogether(other: BitSet) {
    for (i <- 0 until words.length) {
      println(String.format(s"%64s", java.lang.Long.toBinaryString(words(i))).replace(' ', '0'))
      println(String.format(s"%64s", java.lang.Long.toBinaryString(other.words(i))).replace(' ', '0'))
      println
    }
  }

}