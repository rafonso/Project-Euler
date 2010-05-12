package eulerProject

import scala.collection.immutable._

/**
 * Problem 72: How many elements would be contained in the set of reduced 
 * proper fractions for d <= 1,000,000?<br>
 * 18 June 2004<br>
 * <br>
 * Consider the fraction, n/d, where n and d are positive integers. If n &lt; d 
 * and HCF(n,d)=1, it is called a reduced proper fraction.<br>
 * <br>
 * If we list the set of reduced proper fractions for d <= 8 in ascending 
 * order of size, we get:<br>
 * <br>
 * 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 
 * 5/7, 3/4, 4/5, 5/6, 6/7, 7/8<br>
 * <br>
 * It can be seen that there are 21 elements in this set.<br>
 * <br>
 * <b>How many elements would be contained in the set of reduced proper fractions 
 * for d <= 1,000,000?</b><br>
 * <br>
 */
object Problem072 {
  
  def getQuantityOfFractions(max: Int): Long = {
    val primes: List[Long] = Utils.getPrimesUntil(max)
    val primesWith2 = 1L :: primes
    val primesWithout2 = 1L :: primes.tail
    
    /*
    def printsSituation(denominator: Int, quantity: Long) = 
      if(denominator % 1000 == 0) printf("Fractions for %,9d = %,10d%n", denominator, quantity)
    
    def gcd(n: Int, d: Int): Int = d match {
      case 0 => n
      case 1 => 1
      case _ => gcd(d, n % d)
    }
    
    def getFractionsForDenominator(numerator: Int, denominator: Int, step: Int, quantity: Long): Long = 
      if(numerator >= denominator) quantity
//      else if(primes.contains(numerator)) getFractionsForDenominator(numerator + step, denominator, step, quantity + 1)
      else {
        val increment = if(gcd(numerator, denominator) > 1) 0 else 1
        getFractionsForDenominator(numerator + step, denominator, step, quantity + increment)
      }
    
    
    def getFractions(denominator: Int, quantity: Long): Long = {
      if(denominator > max) quantity
      //else if(primes(denominator.longValue)) getFractions(denominator + 1, quantity + denominator - 1)
      else {
        val step = if(denominator % 2 == 0) 2 else 1
        val quantityForDenominator = getFractionsForDenominator(1, denominator, step, 0)
        printsSituation(denominator, quantityForDenominator)
        getFractions(denominator + 1, quantity + quantityForDenominator)
      }
    }
    */
    def getPrimesLetterThan(d: Int): Int = {
      
      def calculate(primes: List[Long], quantity: Int): Int = primes match {
        case prime :: otherPrimes if(prime >= d) => quantity
        case prime :: otherPrimes => 
          print("" + prime + "/" + d + ", ")
          calculate(otherPrimes, quantity + 1)
        case Nil if(d == max) => quantity
        case _ => error("Irregular situation - d = " + d + ", quantity = " + quantity + ", primes = " + primes)
      }
      
      val primesList = if(d % 2 == 0) primesWithout2 else primesWith2
      calculate(primesList, 0)
    }
    
    //getFractions(2, 0)
    (1 to max).foldLeft(0)(_ + getPrimesLetterThan(_))
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 8
    
    val t0 = System.currentTimeMillis
    val result = getQuantityOfFractions(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
