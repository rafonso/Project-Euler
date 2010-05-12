package eulerProject

import scala.collection.immutable._

/**
 * Problem 193: Squarefree Numbers<br/>
 * 10 May 2008<br/>
 * <br/>
 * A positive integer n is called squarefree, if no square of a prime divides 
 * n, thus 1, 2, 3, 5, 6, 7, 10, 11 are squarefree, but not 4, 8, 9, 12<br/>
 * <br/>
 * <b>How many squarefree numbers are there below 2<sup>50</sup>?<b/><br/>
 * <br/>
 */
object Problem193 {
  
  def getSquaresMultiples(max: Long, squareMax: Long): Long = {
    
    def getMultiples(p: Long, multiple: Long, multiples: List[Long]): List[Long] = {
      if(multiple > max) multiples
      else getMultiples(p, multiple + p, multiple :: multiples)
    }
    
    def accumulatePrimesMultiples(primes: List[Long], multiples: Set[Long]): Int = primes match {
      case squarePrime :: others => {
        val qty = max / squarePrime
        val primesMultiple = (1 to qty.toInt).map(_ * squarePrime)
        println("multiples(%,11d) = %,12d".format(squarePrime, primesMultiple.size))
        accumulatePrimesMultiples(others, multiples ++ primesMultiple)
      }
      case Nil => multiples.size
    }
    
    val squarePrimes = Utils.getPrimesUntil(squareMax).map(p => p * p)
    accumulatePrimesMultiples(squarePrimes, Set.empty[Long])
  }
  
  def main(args : Array[String]) : Unit = {
    val squareMax = (1 until 12).foldLeft(2L)((pow, i) => pow * 2)
    val max = squareMax * squareMax
    println("squareMax  = %,d".format(squareMax))
    println("max = %,d".format(max))
    
    val t0 = System.currentTimeMillis
    val squaresQuantity = getSquaresMultiples(max, squareMax) 
    val result = max - squaresQuantity
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println("Maximum      = %,21d".format(max))
    println("Squares      = %,21d".format(squaresQuantity))
    println("Squares Free = %,21d".format(result))
    println("Total Time: " + deltaT + " ms")
  }
}
//1.125.899.906.842.624
//11.258.999