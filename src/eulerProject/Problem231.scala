package eulerProject

/**
 * Problem 231: The prime factorisation of binomial coefficients.<br>
 * 06 February 2009<br>
 * <br>
 * The binomial coefficient ^(10)C_(3) = 120.<br>
 * 120 = 2^(3) × 3 × 5 = 2 × 2 × 2 × 3 × 5, and 2 + 2 + 2 + 3 + 5 = 14.<br>
 * So the sum of the terms in the prime factorisation of ^(10)C_(3) is 14.<br>
 * <br>
 * <b>Find the sum of the terms in the prime factorisation of ^(20000000)C_(15000000).</b><br>
 * 
 * PROBLEMA: DEMORA MUITO!!! combinations(20000000, 15000000) está sendo 
 * calculado há uma hora e ainda não chegou ao fim.
 */
object Problem231 {
  
  
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
  
  /*
    def multiply(acc: Fraction, i: Int, d: Long): Fraction = {
      val newFrac = Frac(i + d, i)
      acc * newFrac
    }
    
  def combination1(n: Long, r: Long): Long = {
    
    val diff = if(2 * r < n) r else n - r
    val result = (1 to diff.toInt).foldLeft(Frac(1, 1))((acc, i) => multiply(acc, i, diff))
      //acc * Frac(i + diff, i))
    
    if(result.denominator != 1) {
      error(result.toString)
    }
    
    result.numerator
  }
  */
  /*
  def multiply(num: Long, den: Long, acc: Fraction): Fraction = 
    if(den == 1) acc * Fraction(num)
    else multiply(num - 1, den - 1, acc * Fraction(num, den))
  
  def combination2(n: Long, r: Long): BigInt = {
    if(n < r) {
      error("n < r")
    }
    if(n == r) {
      return 1
    }
    
    val den = if(2 * r < n) r else n - r
    val result = multiply(n, den, Fraction(1))
    
    if(result.denominator != 1) {
      error(result.toString)
    }
    
    result.numerator
  }
*/
  
  def main(args : Array[String]) : Unit = {
    val n = 200000
    val r = 150000
    val t0 = System.currentTimeMillis
    val result = combinations(n, r)
    val deltaT = System.currentTimeMillis - t0
    
//    println("==================") 17310309456440
    println("C(" + n + ", " + r + ") = " + result)
    println("Time = " + deltaT + " ms")
    
  }
}
