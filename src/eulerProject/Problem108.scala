package eulerProject

/**
 * Problem 108: Solving the Diophantine equation 1 / x + 1 / y = 1 / n.<br>
 * 04 November 2005<br>
 * <br>
 * In the following equation x, y, and n are positive integers.<br>
 * <br>
 * 1 / x + 1 / y = 1 / n <br>
 * <br>
 * For n = 4 there are exactly three distinct solutions:<br>
 * <br>
 * 1 / 5 + 1 / 20 = 1 / 4<br>
 * 1 / 6 + 1 / 12 = 1 / 4<br>
 * 1 / 8 + 1 / 8  = 1 / 4<br>
 * <br>
 * <b>What is the least value of n for which the number of distinct solutions 
 * exceeds one-thousand?</b><br>
 * <br>
 * NOTE: This problem is an easier version of problem 110; it is strongly 
 * advised that you solve this one first.<br>
 * 3850
 */
object Problem108 {
  
  def getDiophantineFactors(n: Int): Int = {
    
    def isExact(x: Int) = ((n * x) % (x - n)) == 0
    def count(acc: Int, x: Int) = if(isExact(x)) acc + 1 else acc
    
    val xMin = n + 1
    val xMax = n * n + n
    val range = (xMin to xMax)
    (xMin to xMax).foldLeft(0)(count(_, _))
  }
  
  def getNumberWithMaxDiophantineSolutions(from: Int, max: Int): Int = {
    
    def eval(i: Int): Int = {
      val quantity = getDiophantineFactors(i)
      println("sol(" + i + ") = " + quantity)
      if(quantity >= max) i
      else eval(i + 1)
    }
    
    eval(from)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = getNumberWithMaxDiophantineSolutions(5902, 1000)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
