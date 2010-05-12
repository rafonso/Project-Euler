package eulerProject.solved

/**
 * Problem 27: Find a quadratic formula that produces the maximum number of 
 * primes for consecutive values of n.<br>
 * 27 September 2002<br>
 * Euler published the remarkable quadratic formula:<br>
 * <br>
 * n^2 + n + 41<br>
 * <br>
 * It turns out that the formula will produce 40 primes for the consecutive 
 * values n = 0 to 39. However, when n = 40, 40^(2) + 40 + 41 = 40(40 + 1) + 41 
 * is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly 
 * divisible by 41.<br>
 * <br>
 * Using computers, the incredible formula  n^2 - 79n + 1601 was discovered, 
 * which produces 80 primes for the consecutive values n = 0 to 79. The 
 * product of the coefficients, -79 and 1601, is -126479.<br>
 * <br>
 * Considering quadratics of the form:<br>
 * <br>
 * n^2 + an + b, where |a| < 1000 and |b| < 1000<br>
 * <br>
 * where |n| is the modulus / absolute value of n<br>
 * e.g. |11| = 11 and |-4| = 4<br>
 * <br>
 * <b>Find the product of the coefficients, a and b, for the quadratic 
 * expression that produces the maximum number of primes for consecutive 
 * values of n, starting with n = 0.</b><br>
 * <br>
 * EULER: SOLVED
 */
object Problem027 {
  
  def getQuantityOfPrimes(a: Int, b: Int, primeIterator: PrimeIterator): Int = {
    
    def eval(n: Int): Int = {
      val value = n * n + a * n + b
      if(value > 0 && primeIterator.isPrime(value)) eval(n + 1)
      else n - 1
    }
    
    eval(1)
  }
  
  def getResult(): (Int, Int, Int) = {
    val it = new PrimeIterator
    it.iterateUntil(1000)
    val bValues = it.getAccumulatedPrimes
//    println(bValues)
//    it.iterateUntil(1000000)
    
    def evaluateA(currentABSize: (Int, Int, Int), _a: Int, signal: Byte): (Int, Int, Int) = {
      val a = signal * _a
      
      def evaluateB(currentBSize: (Int, Int), b: Long): (Int, Int) = {
        val qty = getQuantityOfPrimes(a, b.toInt, it)
//        printf("\tqty(% 3d, %3d) = %,4d%n", a, b, qty)
//        println("\tqty("+ a + ", " + b + ") = " + qty)
        if(qty > currentBSize._2) (b.toInt, qty)
        else currentBSize
      }
      
//      println(a)
      val greatestBSize = bValues.foldLeft((0, 0))(evaluateB(_, _))
//      println("QTY("+ a + ", " + greatestBSize._1 + ") = " + greatestBSize._2)
      if(greatestBSize._2 > currentABSize._3) (a, greatestBSize._1, greatestBSize._2)
      else currentABSize
    }
    
    val positiveResult = (0 until 1000).foldLeft((0, 0, 0))(evaluateA(_, _, 1))
    val negativeResult = (0 until 1000).foldLeft((0, 0, 0))(evaluateA(_, _, -1))
    
    if(positiveResult._3 > negativeResult._3) positiveResult
    else negativeResult
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = getResult
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================")
    println(result)
    println(result._1 * result._2)
    println("Time = " + deltaT + " ms")
  }
}
