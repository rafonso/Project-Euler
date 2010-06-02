package eulerProject

/**
 * Problem 131: Determining primes, p, for which n<sup>3</sup> + n<sup>2</sup>p is a perfect cube.<br>
 * 10 November 2006<br>
 * <br>
 * There are some prime values, p, for which there exists a positive integer, 
 * n, such that the expression n<sup>3</sup> + n<sup>2</sup>p is a perfect cube.<br>
 * <br>
 * For example, when p = 19, 8<sup>3</sup> + 8<sup>2</sup>×19 = 12<sup>3</sup>.<br>
 * <br>
 * What is perhaps most surprising is that for each prime with this property 
 * the value of n is unique, and there are only four such primes below 
 * one-hundred.<br>
 * <br>
 * <b>How many primes below one million have this remarkable property?</b><br>
 * <br>
 */
object Problem131c {
  
  import Utils._
  import scala.collection.immutable.TreeSet 
  
  def evaluateN(n: Int, limit: Int, primes: Set[Long]): Option[Long] = {
    
    val square = n * n
    val cube = square * n
    
    def evaluateK(k: Int, kMax: Int): Option[Long] = {
      if(k > kMax) None
      else {
        val candidate = (k * k * k - cube)/ square
        if(primes(candidate)) Some(candidate)
        else evaluateK(k + 1, kMax)
      }
    }
    
    val kMax = java.lang.Math.cbrt(square * (limit + n)).toInt
    evaluateK(n + 1, kMax)
  }
  
  def getValidPrimes(limit: Int): Set[Long] = {
    
    val primes = Set.empty[Long] + getPrimesUntil(limit)
    
    primes
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val primes = Set.empty[Long] + getPrimesUntil(max)
    val result = getPrimesUntil(max).size
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
