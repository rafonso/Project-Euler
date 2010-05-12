package eulerProject

/**
 * Problem 273: Sum of Squares<br/>
 * 09 January 2010<br/>
 * <br/>
 * Consider equations of the form: a<sup>2</sup> + b<sup>2</sup> = N, 
 * 0 <= a <= b, a, b and N integer.<br/>
 * <br/>
 * For N=65 there are two solutions:<br/>
 * <br/>
 * a = 1, b = 8 and a = 4, b = 7.<br/>
 * <br/>
 * We call S(N) the sum of the values of a of all solutions of 
 * a<sup>2</sup> + b<sup>2</sup> = N, 0 <= a <= b, a, b and N integer.<br/>
 * <br/>
 * Thus S(65) = 1 + 4 = 5.<br/>
 * <br/>
 * <b>Find Sum(S(N)), for all squarefree N only divisible by primes of the 
 * form 4k+1 with 4k+1 < 150.</b><br/>
 * <br/>
 */
object Problem273 {
  
  def s(n: Int): Int = {
    
    /**
     * a^2 + b^2 = N <--> b^2 = N - a^2 <--> b = Sqrt(N - a^2)
     */
    def evaluateA(a: Int, aSum: Int): Int = Utils.getSqrt(n - a * a) match {
      case Left (b) if(b >= a) => evaluateA(a + 1,     aSum)
      case Right(b) if(b >= a) => evaluateA(a + 1, a + aSum)
      case _ => aSum
    }
    
    evaluateA(0, 0)
  }
  
  def getSum(max: Int): Int = {
    
    def isSquareFree(n:Int, squares: List[Long]): Boolean = !squares.exists(n % _ == 0)
    
    def isDivisibleNumber(n: Int, correctPrimes: List[Long]): Boolean = correctPrimes.exists(n % _ == 0)
    
    val primes: List[Long] = Utils.getPrimesUntil(max)
    println("primes = " + primes)
    val squarePrimes: List[Long] = primes.map(p => p * p)
    println("squarePrimes = " + squarePrimes)
    val squareFrees = (1 to max).filter(isSquareFree(_, squarePrimes)).toList
    println("squareFrees = " + squareFrees)
    // p = 4 * k + 1 <--> k = (p - 1) / 4
    val correctPrimes = primes.filter(p => ((p - 1) % 4 == 0))
    println("correctPrimes = " + correctPrimes)
    val divisibleNumbers = squareFrees.filter(isDivisibleNumber(_, correctPrimes))
    println("divisibleNumbers = " + divisibleNumbers)
    val sums = divisibleNumbers.map(s(_))
    println("sums = " + sums)
    
    sums.foldLeft(0)(_ + _)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 150
    
    println(getSum(max))
    
  }
}
