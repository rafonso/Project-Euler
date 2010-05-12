package eulerProject

import scala.collection.immutable.Set
import Utils._

/**
 * Problem 131: Determining primes, p, for which n^3 + n^2*p is a perfect cube.<br>
 * 10 November 2006<br>
 * <br>
 * There are some prime values, p, for which there exists a positive integer, 
 * n, such that the expression n^(3) + n^(2)p is a perfect cube.<br>
 * <br>
 * For example, when p = 19, 8^(3) + 8^(2)×19 = 12^(3).<br>
 * <br>
 * What is perhaps most surprising is that for each prime with this property 
 * the value of n is unique, and there are only four such primes below 
 * one-hundred.<br>
 * <br>
 * How many primes below one million have this remarkable property?<br>
 * <br>
 */
object Problem131b {
  
  def testN(n: Long, max: Long, primes: Set[Long]): Either[Double, Long] = {
    
    val nSquare = n * n
    val nCube = nSquare * n
    val kMin = n + 1
    val kMax = java.lang.Math.cbrt(nSquare * (n + max)).toLong
    
    def eval(k: Long): Either[Double, Long] = {
      if(k < kMax) {
        val numerator = (k * k * k) - nCube
        val (quocient, remainder) = /%(numerator, nSquare)
        println("k = %,6d, kMax = %,6d, numerator = %,6d, quocient = %,6d, remainder = %,6d".format(k, kMax, numerator, quocient, remainder))
        if(remainder == 0 && primes(quocient)) Right(quocient)
        else eval(k + 1)
      } else {
        Left(((k * k * k) - nCube).toDouble / nSquare)
      }
    }
    
    eval(kMin)
  }
  
  def getPrimes(max: Long): List[Long] = {
    
    val primes = Set.empty[Long] ++ getPrimesUntil(max)
    
    def evalN(n: Long, priorP: Double, selectedPrimes: List[Long]): List[Long] = {
      println("* n = %,9d".format(n))
      testN(n, max, primes) match {
        case Right(p) if(p >= priorP) => {
          println("n = %,9d, p = %,9d".format(n, p))
          evalN(n + 1, p, p :: selectedPrimes)
        }
        case Right(p) if(p <  priorP) => selectedPrimes
        case Left (x) if(x >= priorP) => evalN(n + 1, x, selectedPrimes)
        case Left (x) if(x <  priorP) => selectedPrimes
        case _ => error("n = " + n)
      }
    }
    
    evalN(1, 0, Nil)
  }
  
  
  def main(args : Array[String]) : Unit = {
    
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val selectedPrimes = getPrimes(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(selectedPrimes)
    println("Size: " + selectedPrimes.size)
    println("Total Time: " + deltaT + " ms")
    /*
    val max = 1000
    val primes = Set.empty[Long] ++ getPrimesUntil(max)
    val n = 5000
    
    println(testN(n, max, primes))
    */
  }
}
