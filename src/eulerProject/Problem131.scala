package eulerProject

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
object Problem131 {
  
  def getCubicRoot(a: Long): Long = {
    
    def hayleyMethod(xAnt: BigInt): BigInt = {
      val cube = xAnt * xAnt * xAnt
      val x = xAnt * (cube + 2 * a) / (cube * 2 + a)
      
      if(x == xAnt) x
      else hayleyMethod(x)
    }
    
    hayleyMethod(a).longValue
  }
  
  def solveCubic(prime: Long): Option[(Long, Long)] = {
    
    def evaluate(k: Long): Option[(Long, Long)] = {
      val n = k * k * k
      if(n > 2 * prime) None
      else {
        val value = n * n * (n + prime)
        val cubicRoot = getCubicRoot(value)
        if((cubicRoot * cubicRoot * cubicRoot) == value) Some((prime, n))
        else evaluate(k + 1)
      }
    }
    
    evaluate(1)
  }
  
  def getSolutions(max: Long): List[(Long, Long)] = {
    
    def calculate(primes: List[Long], solutions: List[(Long, Long)]): List[(Long, Long)] = primes match {
      case Nil => solutions
      case prime :: otherPrimes => 
        val solution = solveCubic(prime)
        if(solution.isDefined) {
          println(solution.get)
          calculate(otherPrimes, solution.get :: solutions)
        }
        else calculate(otherPrimes, solutions)
    }
    
    println("Getting primes until " + max)
    val t0 = System.currentTimeMillis
    val primes = Utils.getPrimesUntil(max)
    val deltaT = System.currentTimeMillis - t0
    println("There are " + primes.size + " primes until " + max + ". Time: " + deltaT + " ms")
    
    calculate(primes, Nil).reverse
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 10000000
    
    val t0 = System.currentTimeMillis
    val result = getSolutions(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
//    println(result)
    println("Size: " + result.size)
    println("Total Time: " + deltaT + " ms")
  }
}
