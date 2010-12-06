package eulerProject.solved

import scala.collection.immutable.Queue

/**
 * Problem 125: Finding square sums that are palindromic.<br/>
 * 04 August 2006<br/>
 * <br/>
 * The palindromic number 595 is interesting because it can be written as the 
 * sum of consecutive squares: 6<sup>2</sup> + 7<sup>2</sup> + 8<sup>2</sup> 
 * + 9<sup>2</sup> + 10<sup>2</sup> + 11<sup>2</sup> + 12<sup>2</sup>.<br/>
 * <br/>
 * There are exactly eleven palindromes below one-thousand that can be written 
 * as consecutive square sums, and the sum of these palindromes is 4164. Note 
 * that 1 = 0<sup>2</sup> + 1<sup>2</sup> has not been included as this problem 
 * is concerned with the squares of positive integers.<br/>
 * <br/>
 * <b>Find the sum of all the numbers less than 10<sup>8</sup> that are both 
 * palindromic and can be written as the sum of consecutive squares.</b><br/>
 * <br/>
 */
object Problem125 {
  
  def isQuadraticSum(n: Int): Boolean = {
    
    def printTerms(terms: Queue[Int]) {
      print("%,11d (%3d)= ".format(n, terms.size))
      terms.reverse.foreach(t => print("%,5d^2 + ".format(t)))
      println
    }

    def getTerms(term: Int, priorSum: Long, accumulatedTerms: Queue[Int]): Option[Queue[Int]] = {
//      println(priorSum + " -> " + accumulatedTerms)
      val sum = priorSum + (term * term)
      if(sum == n) Some(accumulatedTerms.enqueue(term))
      else if(sum < n) {
        if(term <= 1) None
        else getTerms(term - 1, sum, accumulatedTerms.enqueue(term))
      } else {
        val (first, nextTerms) = accumulatedTerms.dequeue
        getTerms(term, priorSum - (first * first), nextTerms)
      }
    }
  
    val optTerms = getTerms(Math.sqrt(n).toInt, 0, Queue.Empty)
    if(optTerms.isDefined) {
      if(optTerms.get.size == 1) false
      else {
        printTerms(optTerms.get)
        true
      }
    } else false
  }
  
  def isPalidrome(n: Int): Boolean = {
    
    def getReverse(base: Int, reverse: Int): Int = {
      if(base == 0) reverse
      else getReverse(base / 10, reverse * 10 + base % 10)
    }
    
    (n == getReverse(n, 0))
  }


  def main(args : Array[String]) : Unit = {
    val max = 100000000
    
    val t0 = System.currentTimeMillis
    val result = (2 to max).filter(isPalidrome(_)).filter(isQuadraticSum(_))
    val sum = result.foldLeft(0L)(_ + _)
    val deltaT = System.currentTimeMillis - t0
    
    println("============================================")
    println(result)
    println(sum)
    println("Time = " + deltaT + " ms")
  }
}
