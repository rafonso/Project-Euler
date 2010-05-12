package eulerProject.solved

/**
 * Problem 34: Find the sum of all numbers which are equal to the sum of the 
 * factorial of their digits.<br>
 * 03 January 2003<br>
 * <br>
 * 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.<br>
 * <br>
 * Find the sum of all numbers which are equal to the sum of the factorial of
 * their digits.<br>
 * <br>
 * Note: as 1! = 1 and 2! = 2 are not sums they are not included.<br>
 * 
 * @author rafael
 * EULER: SOLVED
 */
object Probleam034 {
  
  val digitsFactorial = Array(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
  
  def isSumOfFactorialDigits(n: Int) = {
    
    def sumFactorialDigits(x: Int, sum: Int): Int = 
      if(x == 0) sum
      else sumFactorialDigits(x / 10, digitsFactorial(x % 10) + sum)
    
    n == sumFactorialDigits(n, 0)
  }
  
  def sumNumbers(i: Int, sum: Int, max: Int): Int = {
    if(i == max) sum
    else if(isSumOfFactorialDigits(i)) {
      println(i)
      sumNumbers(i + 1, i + sum, max)
    } else {
      sumNumbers(i + 1, sum, max)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 10000000
    
    val t0 = System.currentTimeMillis
    val sum = sumNumbers(3, 0, max)
    val deltaT = System.currentTimeMillis - t0
    
    println("========================")
    println("SUM(" + max + ") = " + sum)
    println("TIME: " + deltaT + " ms")
    
  }
}
