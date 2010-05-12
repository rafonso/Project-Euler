package eulerProject

/**
 * Problem 179: Consecutive positive divisors <br>
 * 26 January 2008<br>
 * <br>
 * <b>Find the number of integers 1 < n < 10^(7), for which n and n + 1 have 
 * the same number of positive divisors. For example, 14 has the positive 
 * divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.</b><br>
 * 
 */
object Problem179 {
  
  
  
  def getConsecutivePositiveDivisors(max: Int): Int = {
    
    def getQuantityOfDivisors(n: Int): Int = (1 to n / 2).filter(n % _ == 0).foldLeft(1)((acc, i) => acc + 1)
    
    def print(i: Int, qty: Int) = if(i % 1000 == 0) printf("%,10d => %,10d%n", i, qty)
    
    def evaluate(i: Int, priorQuantity: Int, sum: Int): Int = {
      if(i > max) sum
      else {
        val qty = getQuantityOfDivisors(i)
//        print(i, sum)
        if(qty == priorQuantity) {
          printf("\t(%,10d, %,10d) => %,8d%n", i - 1, i, qty)
          evaluate(i + 1, qty, sum + 1)
        }
        else evaluate(i + 1, qty, sum)
      }
    }
    
    evaluate(2, 0, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = getConsecutivePositiveDivisors(5000)
    val deltaT = System.currentTimeMillis - t0
    
    println("=================================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
