package eulerProject.solved

/**
 * Problem 43: Find the sum of all pandigital numbers with an unusual 
 * sub-string divisibility property.<br/>
 * 09 May 2003<br/>
 * <br/>
 * The number, 1406357289, is a 0 to 9 pandigital number because it is made up 
 * of each of the digits 0 to 9 in some order, but it also has a rather 
 * interesting sub-string divisibility property.<br/>
 * <br/>
 * Let d_(1) be the 1^(st) digit, d_(2) be the 2^(nd) digit, and so on. 
 * In this way, we note the following:<br/>
 * <br/>
 * <ul>
 * <li>d(2)d(3)d(4) = 406 is divisible by  2</li>
 * <li>d(3)d(4)d(5) = 063 is divisible by  3</li>
 * <li>d(4)d(5)d(6) = 635 is divisible by  5</li>
 * <li>d(5)d(6)d(7) = 357 is divisible by  7</li>
 * <li>d(6)d(7)d(8) = 572 is divisible by 11</li>
 * <li>d(7)d(8)d(9) = 728 is divisible by 13</li>
 * <li>d(8)d(9)d(10)= 289 is divisible by 17</li>
 * </ul>
 * <br/>
 * <b>Find the sum of all 0 to 9 pandigital numbers with this property.</b><br/>
 * 
 */
object Problem043 {
  
  def isValid(strNum: String): Boolean = {
    
    def test(from: Int, until: Int, divisor: Int): Boolean = {
      val dividend = strNum.slice(from, until).toInt
      (dividend % divisor) == 0
    }
    
    test(7, 10, 17) && test(6, 9, 13) && test(5, 8, 11) && test(4, 7, 7) && test(3, 6, 5) && test(2, 5, 3) && test(1, 4, 2)
  }
  
  def getValidPermutations: List[Long] = {
    
    def getPermutations(digits: List[Int], value: Long, validPermutations: List[Long]): List[Long] = digits match {
      case Nil => {
        if(isValid("%010d".format(value))) {
          println(value)
          value :: validPermutations
        } else {
          validPermutations
        }
      }
      case _ => digits.flatMap(d => getPermutations(digits.remove(_ == d), value * 10 + d, validPermutations))
    }
    
    getPermutations(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 0, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = getValidPermutations
    val sum = result.foldLeft(0L)(_ + _)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println(sum)
    println("Total Time: " + deltaT + " ms")
  }
}
