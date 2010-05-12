package eulerProject

/**
 * Problem 80: Calculating the digital sum of the decimal digits of irrational 
 * square roots.<br>
 * 08 October 2004<br>
 * <br>
 * It is well known that if the square root of a natural number is not an 
 * integer, then it is irrational. The decimal expansion of such square roots 
 * is infinite without any repeating pattern at all.<br>
 * <br>
 * The square root of two is 1.41421356237309504880..., and the digital sum 
 * of the first one hundred decimal digits is 475.<br>
 * <br>
 * <b>For the first one hundred natural numbers, find the total of the digital 
 * sums of the first one hundred decimal digits for all the irrational square 
 * roots.</b><br>
 * <br>
 */
object Problem080 {
  
  def getPrecision(p: Int): BigDecimal = {
    
    def calculate(precision: BigDecimal, digits: Int): BigDecimal =
      if(digits >= p) precision
      else calculate(precision / 10, digits + 1)
    
    calculate(BigDecimal(1).setScale(p), 0)
  }
  
  
  def getSqrt(n: Int, precision: Int): BigDecimal = {
    val bigPrecision = getPrecision(precision)
    val bigN = BigDecimal(n.toDouble).setScale(precision)
    
    def calculate(root: BigDecimal): BigDecimal = {
      println(root)
      val nextRoot = (root * root + bigN).setScale(precision) / (root * 2)
      if(nextRoot == root) root
      else calculate(nextRoot)
    }
    
    calculate(BigDecimal(Math.sqrt(n.toDouble)).setScale(precision))
  }
  
  def main(args : Array[String]) : Unit = {
    println(getSqrt(2, 100))
    
  }
}
