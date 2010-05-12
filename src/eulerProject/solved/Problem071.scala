package eulerProject.solved

/**
 * Problem 71: Listing reduced proper fractions in ascending order of size. <br>
 * 04 June 2004<br>
 * <br>
 * Consider the fraction, n/d, where n and d are positive integers. If n<d 
 * and HCF(n,d)=1, it is called a reduced proper fraction.<br>
 * <br>
 * If we list the set of reduced proper fractions for d <= 8 in ascending 
 * order of size, we get:<br>
 * <br>
 * 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, <b>3/7</b>, 1/2, 4/7, 3/5, 5/8, 
 * 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8<br>
 * <br>
 * It can be seen that 2/5 is the fraction immediately to the left of 3/7.<br>
 * <br>
 * <b>By listing the set of reduced proper fractions for d <= 1,000,000 in 
 * ascending order of size, find the numerator of the fraction immediately to 
 * the left of 3/7.</b><br>
 * <br>
 * EULER: SOLVED
 */
object Problem071 {

  def gcd(n: Int, d: Int): Int = d match {
    case 0 => n
    case 1 => 1
    case _ => gcd(d, n % d)
  }
  
  def getGreatestFractionSmallerThan(baseFraction: (Int, Int), maxDenominator: Int): (Int, Int, Double) = {
    
    def calculate(denominator: Int, fraction: (Int, Int, Double)): (Int, Int, Double) = {
      if(denominator > maxDenominator) fraction
      else if(baseFraction._2 % denominator == 0) calculate(denominator + 1, fraction)
      else {
        val numerator = baseFraction._1 * denominator / baseFraction._2
        if(gcd(denominator, numerator) > 1) {
          calculate(denominator + 1, fraction)
        } else {
          val doubleValue = numerator.doubleValue / denominator
          if(doubleValue > fraction._3) {
            println(numerator + "/" + denominator + " = " + doubleValue)
            calculate(denominator + 1, (numerator, denominator, doubleValue))
          } else {
            calculate(denominator + 1, fraction)
          }
        }
      }
    }
    
    calculate(1, (0, 1, 0))
  }
  
  def main(args : Array[String]) : Unit = {
    val n = 3
    val d = 7
    val max = 1000000
    
    val t0 = System.currentTimeMillis
    val result = getGreatestFractionSmallerThan((n, d), max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
