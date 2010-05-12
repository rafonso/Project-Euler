package eulerProject

/**
 * Problem 134: Finding the smallest positive integer related to any pair of 
 * consecutive primes.<br/>
 * 15 December 2006<br/>
 * <br/>
 * Consider the consecutive primes p<sub>1</sub> = 19 and p<sub>2</sub> = 23. It 
 * can be verified that 1219 is the smallest number such that the last digits 
 * are formed by p<sub>1</sub> whilst also being divisible by p<sub>2</sub>.<br/>
 * <br/>
 * In fact, with the exception of p<sub>1</sub> = 3 and p<sub>2</sub> = 5, for 
 * every pair of consecutive primes, p<sub>2</sub> > p<sub>1</sub>, there exist 
 * values of n for which the last digits are formed by p<sub>1</sub> and n is 
 * divisible by p<sub>2</sub>. Let S be the smallest of these values of n.<br/>
 * <br/>
 * <b>Find Sum(S) for every pair of consecutive primes with 
 * 5 <= p<sub>1</sub> <= 1000000.</b><br/>
 * <br/>
 */
object Problem134 {
  
  val powers10: Array[Long] = Array(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)
  
  def findMultiple(p1: Long, p2: Long): Long = {
    val pow10 = powers10(p2.toString.size)
    
    def find(multipleP2: Long): Long = {
      if((multipleP2 % pow10) == p1) multipleP2 
      else find(multipleP2 + p2)
    }
    
    find(p2)
  }
  
  def getSum(max: Long): BigInt = {
    
    def getPrimes = {
      val originalPrimes = Utils.getPrimesUntil((max * 1.1).toLong)
      val (smallerPrimes, greaterPrimes) = originalPrimes.span(_ <= max)
      smallerPrimes match {
        case 2 :: 3 :: others if(others.last == max) => others 
        case 2 :: 3 :: others if(others.last < max) => others ::: List(greaterPrimes.first)
        case _ => error(smallerPrimes.toString)
      }
    }
    
    def sumMultiples(primes: List[Long], sum: BigInt): BigInt = primes match {
      case p1 :: p2 :: others => {
        val s = findMultiple(p1, p2)
        println("s(%,7d, %,9d) = %,17d".format(p1, p2, s))
        sumMultiples(p2 :: others, sum + s)
      }
      case _ :: Nil => sum
      case _ => error(primes.toString)
    }
    
    sumMultiples(getPrimes, BigInt(0))
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000000
    
    val t0 = System.currentTimeMillis
    val result = getSum(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}