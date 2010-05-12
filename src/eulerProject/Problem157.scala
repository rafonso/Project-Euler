package eulerProject

/**
 * Problem 157: Solving the diophantine equation 1 / a + 1 / b = p / 10<sup>n</sup>.<br/>
 * 01 June 2007<br/>
 * <br/>
 * Consider the diophantine equation 1/ a + 1 / b = p /10^(n) with a, b, p, n positive integers and a <= b.<br/>
 * For n = 1 this equation has 20 solutions that are listed below:<br/>
 * <table border="1">
 * <tr><td>1 /  1 + 1 /   1 = 20 / 10</td><td>1 /  1 + 1 /  2 = 15 / 10</td><td>1 /  1 + 1 /  5 = 12 / 10</td><td>1 /  1 + 1 / 10 = 11 / 10</td><td>1 /  2 + 1 /  2 = 10 / 10</td></tr>
 * <tr><td>1 /  2 + 1 /   5 =  7 / 10</td><td>1 /  2 + 1 / 10 =  6 / 10</td><td>1 /  3 + 1 /  6 =  5 / 10</td><td>1 /  3 + 1 / 15 =  4 / 10</td><td>1 /  4 + 1 /  4 =  5 / 10</td></tr>
 * <tr><td>1 /  4 + 1 /  20 =  3 / 10</td><td>1 /  5 + 1 /  5 =  4 / 10</td><td>1 /  5 + 1 / 10 =  3 / 10</td><td>1 /  6 + 1 / 30 =  2 / 10</td><td>1 / 10 + 1 / 10 =  2 / 10</td></tr>
 * <tr><td>1 / 11 + 1 / 110 =  1 / 10</td><td>1 / 12 + 1 / 60 =  1 / 10</td><td>1 / 14 + 1 / 35 =  1 / 10</td><td>1 / 15 + 1 / 30 =  1 / 10</td><td>1 / 20 + 1 / 20 =  1 / 10</td></tr>
 * </table>
 * <br>
 * <b>How many solutions has this equation for 1 <= n <= 9?</b><br/>
 * 
 */
object Problem157 {
  
  def getSolutionsForP(pow10: Int, p: Int): Int = {
    
    def getAMin(aCand: Int): Int = if(aCand * p >= (pow10 + 1)) aCand else getAMin(aCand + 1) 
    
    def isValidA(a: Long) = (((a * pow10) % (a * p - pow10)) == 0)
  
    def evaluateA(acc: Int, a: Int): Int = {
//      print(" [" + a + "]")
      if(isValidA(a)) {
//        val b = ((a.toLong * pow10) / (a * p - pow10))
//        print("(" + a + ", " + b + ", " + p + ") ")
        acc + 1
      } else {
        acc
      }
    }
  
    val aMin = getAMin(1) 
    val aMax = (2 * pow10) / p
//    print("\n\tp = " + p + " => ")
    (aMin to aMax).foldLeft(0)(evaluateA(_, _))
  }
  
  def getSolutionsForN(n: Int): Int = {
    
    val pow10 = (1 to n).foldLeft(1)((acc, i) => acc * 10)
    // 1 / 1 + 1 / 1 = pMax / pow10 <--> 2 * pow10 = pMax
    val pMax = 2 * pow10
    
//    print("\nn = " + n + " (" + pow10 + ") => ")
    printf("%nn = %d (%10d)", n, pow10)
    (1 to pMax).foldLeft(0)(_ + getSolutionsForP(pow10, _))
  }
  
  def main(args : Array[String]) : Unit = {
    val min = 1
    val max = 9
    val t0 = System.currentTimeMillis
    val result = (min to max).foldLeft(0)(_ + getSolutionsForN(_))
    val deltaT = System.currentTimeMillis - t0
    
    println()
    println("=========================================================")
    println("Result from " + min + " to " + max + " = " + result)
    println("Time = " + deltaT + " ms")
  }
}
/**
 
n = 1 (        10)
n = 2 (       100)
n = 3 (      1000)
n = 4 (     10000)
n = 5 (    100000)
n = 6 (   1000000)
n = 7 (  10000000)
n = 8 ( 100000000)
n = 9 (1000000000)
=========================================================
Result from 1 to 9 = 53490
Time = 4834769 ms

 */