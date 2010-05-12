package eulerProject

/**
 * Problem 160: Factorial trailing digits<br>
 * 07 September 2007<br>
 * <br>
 * For any N, let f(N) be the last five digits before the trailing zeroes in N!.<br>
 * For example,<br>
 * <br>
 * 9! = 362880 so f(9) = 36288<br>
 * 10! = 3628800 so f(10) = 36288<br>
 * 20! = 2432902008176640000 so f(20) = 17664<br>
 * <br>
 * <b>Find f(1,000,000,000,000)</b><br>
 * <br>
 */
object Problem160 {
  
  def print(n: Number, fn: Number) = {
    val now = new java.util.Date
    printf("[%tH:%tM:%tS,%tL] f(%,18d) = %,06d%n", now, now, now, now, n.longValue, fn.longValue)
  }
  
  def f(n: BigInt, digits: Int): BigInt = {
    
    val pow10 = (1 to digits).foldLeft(1)((pow, i) => pow * 10)
    val interval = n / 100000
    
    def getLastNDigits(x: BigInt) = x % pow10 
    
    def trimZeroes(x: BigInt): BigInt = if(x % 10 == 0) trimZeroes(x / 10) else x
    
    def calculateProductFor(x: BigInt, product: BigInt): BigInt = {
      val newProduct = x * product
      if(x > n) product
      else calculateProductFor(x * 10, newProduct)
    }
    
    def calculate(product: BigInt, i: BigInt): BigInt = 
      if(i > n) product
    /*
      else {
        val value = getLastNDigits(trimZeroes(product * i))
        calculate(value, i + 1)
      }
*/
  
      else if(i % 10 == 0) {
//        print(i - 1, product)
        calculate(product, i + 1)
      } else {
        val value = getLastNDigits(trimZeroes(product * calculateProductFor(i, 1)))
//        if(i % interval == 0) print(i, value) 
        calculate(value, i + 1)
      }
    
    calculate(1, 1)
  }
  
  def main(args : Array[String]) : Unit = {
    val n = 1000L
      // 10.000.000L
    val t0 = System.currentTimeMillis
    val result = f(n, 5)
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================================================")
    print(n, result.longValue)
    println("Time = " + deltaT + " ms")
  }
}
