package eulerProject.solved

/**
 * Problem 30: Find the sum of all the numbers that can be written as the sum 
 * of fifth powers of their digits.<br>
 * 08 November 2002<br>
 * <br>
 * Surprisingly there are only three numbers that can be written as the sum 
 * of fourth powers of their digits:
 * <br>
 * 1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)<br>
 * 8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)<br>
 * 9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)<br>
 * <br>
 * As 1 = 1^(4) is not a sum it is not included.<br>
 * <br>
 * The sum of these numbers is 1634 + 8208 + 9474 = 19316.<br>
 * <br>
 * <b>Find the sum of all the numbers that can be written as the sum of fifth 
 * powers of their digits.</b><br>
 * <br>
 * EULER: SOLVED
 */
object Problem030 {
  
  def isSumPower(x: Int, n: Int): Boolean = {
    val powers = (0 to 9).map(d => (1 to n).foldLeft(1)((product, i) => product * d)).toArray
    
    def sumPowDigits(number: Int, sum: Int): Int = 
      if(number == 0) sum
      else sumPowDigits(number / 10, sum + powers(number % 10))
    
//    val res = sumPowDigits(x, 0)
//    println(x + " -> " + res)
    x == sumPowDigits(x, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000000
    val exp = 5
    
    val t0 = System.currentTimeMillis
    val numbers = for(i <- 2 to max; if isSumPower(i, exp)) yield i
    val sum = numbers.foldLeft(0)(_ + _)
    println(numbers)
    println("SUM(" + max + ") = " + sum)
    val deltaT = System.currentTimeMillis - t0
    
    println("TIME: " + deltaT + " ms")

  }
}
