package eulerProject.solved

/**
 * Problem 123: Determining the remainder when 
 * (p<sub>n</sub> - 1)<sup>n</sup> + (p<sub>n</sub> + 1)<sup>n</sup> 
 * is divided by p<sub>n</sub><sup>2</sup>.<br>
 * 16 June 2006<br>
 * <br>
 * Let p<sub>n</sub> be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the 
 * remainder when 
 * (p<sub>n</sub> - 1)<sup>n</sup> + (p<sub>n</sub> + 1)<sup>n</sup>
 * is divided by p<sub>n</sub><sup>2</sup>.<br>
 * <br>
 * For example, when n = 3, p<sub>3</sub> = 5, and 
 * 4<sup>3</sup> + 6<sup>3</sup> = 280 = 5 mod 25.<br>
 * <br>
 * The least value of n for which the remainder first exceeds 
 * 10<sup>9</sup> is 7037.<br>
 * <br>
 * <b>Find the least value of n for which the remainder first exceeds 
 * 10<sup>10</sup>.</b><br>
 * <br>
 */
object Problem123 {
  
  val ONE = BigInt(1)
  
  def pow(base: Long, expoent: Int): BigInt = (1 to expoent).foldLeft(ONE)((acc, i) => acc * base)
  
  def r(p: Long, n: Int): BigInt = (pow(p - 1, n) + pow(p + 1, n)) % pow(p, 2)
  
  def getResult(min: Long): Int = {
    
    println("getting primes")
    // I read all primes written in a text file. 
    val primes = Utils.getPrimesUntil
    println("I have " + primes.size + " primes")

    /**
     * I add steps' first value to n and I evaluate r for n and prime number in 
     * position (n - 1) in primes list. If r value is greater than min parameter,
     * I back to priorN, add next step in steps list.
     */
    def evaluate(n: Int, priorN: Int, steps: List[Int]): Int = {
      val rn = r(primes(n - 1), n).longValue
      println("r(%,10d) = %,15d".format(n, rn))
      if(rn > min) steps match {
        case List(1) => n
        case currentStep :: nextStep :: others => evaluate(priorN + nextStep, priorN, nextStep :: others)
      } else steps match {
        case currentStep :: others => evaluate(n + currentStep, n, currentStep :: others)
        case Nil => error("No steps")
      }
    }
    
    evaluate(1, 0, List(10000, 1000, 100, 10, 1))
  }
  
  def main(args : Array[String]) : Unit = {
    val min = 10000000000L
    
    val t0 = System.currentTimeMillis
    val result = getResult(min)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
  
}
