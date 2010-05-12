package eulerProject

/**
 * Problem 104: Finding Fibonacci numbers for which the first and last nine 
 * digits are pandigital.<br/>
 * 09 September 2005<br/>
 * <br/>
 * The Fibonacci sequence is defined by the recurrence relation:<br/>
 * <br/>
 * F(n) = F(n-1) + F(n-2), where F(1) = 1 and F(2) = 1.<br/>
 * <br/>
 * It turns out that F_(541), which contains 113 digits, is the first Fibonacci 
 * number for which the last nine digits are 1-9 pandigital (contain all the 
 * digits 1 to 9, but not necessarily in order). And F_(2749), which contains 
 * 575 digits, is the first Fibonacci number for which the first nine 
 * digits are 1-9 pandigital.<br/>
 * <br/>
 * <b>Given that F_(k) is the first Fibonacci number for which the first nine 
 * digits AND the last nine digits are 1-9 pandigital, find k.</b><br/>
 * <br/>
 */
object Problem104 {
  
  def isPandigital(strNum: String): Boolean = {
    
    def verify(digits: List[Char], foundDigits: List[Char]): Boolean = digits match {
      case Nil => foundDigits.size == 9
      case '0' :: others => false
      case d :: others if(foundDigits.contains(d)) => false
      case d :: others => verify(others, d :: foundDigits)
    }
    
    verify(strNum.toList, Nil)
  }
  
  def verifyFibonacci(n: Int, fn1: BigInt, fn2: BigInt): Int = {
    
    def isValid(strNum: String) = isPandigital(strNum.slice(0, 9)) && isPandigital(strNum.drop(strNum.size - 9))
    
    val fn = fn1 + fn2
    val strFn = fn.toString
    if(n % 10000 == 0) println("F(%,8d) (%,7d) = %s + %s".format(n, strFn.size, fn1, fn2))
    
    if(isValid(strFn)) n
    else verifyFibonacci(n + 1, fn, fn1)
  }
  
  def main(args : Array[String]) : Unit = {
    val ONE = BigInt(1)
    val t0 = System.currentTimeMillis
    val result = verifyFibonacci(3, ONE, ONE)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
