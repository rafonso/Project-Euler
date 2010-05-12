package eulerProject.solved

/**
 * Problem 53: How many values of C(n,r), for 1 <= n <= 100, exceed one-million?.<br>
26 September 2003<br>
<br>
There are exactly ten ways of selecting three from five, 12345:<br>
<br>
123, 124, 125, 134, 135, 145, 234, 235, 245, and 345<br>
<br>
In combinatorics, we use the notation, ^(5)C_(3) = 10.<br>
<br>
In general,<br>
^(n)C_(r) = n! / (r! * (n-r)!)
	,where r <= n, n! = n * (n-1) * ... * 3 * 2 * 1, and 0! = 1.

It is not until n = 23, that a value exceeds one-million: ^(23)C_(10) = 1144066.<br>
<br>
How many, not necessarily distinct, values of  ^(n)C_(r), for 1 <= n <= 100, are greater than one-million?<br>
 * EULER: SOLVED
 */
object Problem053 {
  
  val ONE = BigInt(1)
  
  def productory(start: Long, end: Long): BigInt = {
    
    def calculate(current: Long, max: Long, product: BigInt): BigInt = 
      if(current == max) 
        product * current
      else 
        calculate(current + 1, max, product * current)
    
    calculate(start, end, ONE)
  }
  
  def factorial(n: Long): BigInt = productory(1, n)
  
  /**
   n! / (r! * (n-r)!) = 
   n * (n - 1) * (n - 2) * ... * (r + 1) * (r)! / (r! * (n - r)!) =
   n * (n - 1) * (n - 2) * ... * (r + 1) / (n - r)! = 
   prod(r + 1, n) / prod(1, r)
   */
  def combinations(n: Long, r: Long): BigInt = if(n == r) ONE else productory(r + 1, n) / factorial(n - r)
  
  val min = BigInt("1000000")
  val iMax = 100

  def getCombinationsGreaterThanMillion(): Int = {
    
    def getCombinationsForN(n: Long, r: Long, qty: Int): Int = {
      if(r > n) {
        qty
      } else {
        val result = combinations(n, r)
        if(result > min) {
          printf("C(%3d, %3d) = %s %n", n, r, result.toString)
          getCombinationsForN(n, r + 1, qty + 1)
        } else {
          getCombinationsForN(n, r + 1, qty)
        }
      }
    }
    
    def calculate(i: Int, quantity: Int): Int = {
      if(i > iMax) {
        quantity
      } else {
         calculate(i + 1, getCombinationsForN(i, 0, 0) + quantity) 
      }
    }
    
    calculate(0, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = getCombinationsGreaterThanMillion()
    val deltaT = System.currentTimeMillis - t0
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
