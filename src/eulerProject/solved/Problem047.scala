package projectEuler

import scala.annotation.tailrec

/**
 * Problem 47: Find the first four consecutive integers to have four distinct primes factors.<br/>
 * 04 July 2003<br/
 * <br/
 * The first two consecutive numbers to have two distinct prime factors are:<br/
 * <pre>
 * 14 = 2 × 7
 * 15 = 3 × 5
 * </pre>
 * The first three consecutive numbers to have three distinct prime factors are:<br/
 * <pre>
 * 644 = 2² × 7 × 23
 * 645 = 3 × 5 × 43
 * 646 = 2 × 17 × 19.
 * </pre>
 * <b>Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?</b>
 *
 */
object Problem047 extends App {

  def verifyPrimeFactors(n: Long, desirableQuantity: Int, generator: PrimesGenerator): Option[Set[Long]] = {

      def getResult(primesFactors: Set[Long]) = if (primesFactors.size == desirableQuantity) Some(primesFactors) else None

      @tailrec
      def evaluate(x: Long, primes: List[Long], primesFactors: Set[Long]): Option[Set[Long]] = x match {
        case 1 => getResult(primesFactors)
        case x1 => primes match {
          case Nil => getResult(primesFactors)
          case p :: others if (x % p == 0) => {
            val nextPrimes = primesFactors + p
            if (nextPrimes.size > desirableQuantity) None
            else evaluate(x / p, primes, nextPrimes)
          }
          case p :: others => evaluate(x, others, primesFactors)
        }
      }

    evaluate(n, generator.takeUntil(n).toList, Set.empty)
  }

  @tailrec
  def getConsecutiveSequence(n: Long, requiredSizeSequence: Int, desirableQuantity: Int, generator: PrimesGenerator, sequence: List[Long] = Nil): List[Long] = {
    verifyPrimeFactors(n, desirableQuantity, generator) match {
      case None => getConsecutiveSequence(n + 1, requiredSizeSequence, desirableQuantity, generator)
      case Some(factors) => {
        println("%,10d => %s".format(n, factors))
        val nextSequence = n :: sequence
        if (nextSequence.size == requiredSizeSequence) nextSequence
        else getConsecutiveSequence(n + 1, requiredSizeSequence, desirableQuantity, generator, nextSequence)
      }
    }
  }

  /**
   *
   */
  //  def main(args: Array[String]): Unit = {
  val requiredSizeSequence = args(0).toInt
  val desirableQuantity = args(1).toInt

  val result = getConsecutiveSequence(1L, requiredSizeSequence, desirableQuantity, new PrimesGenerator)
  println("")
  println(result)
  //  }
}
