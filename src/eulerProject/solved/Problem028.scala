package eulerProject.solved

/**
 * Problem 28: What is the sum of both diagonals in a 1001 by 1001 spiral?<br/>
 * 11 October 2002<br/>
 * <br/>
 * Starting with the number 1 and moving to the right in a clockwise direction 
 * a 5 by 5 spiral is formed as follows:<br/>
 * <pre>
 * 21 22 23 24 25
 * 20  7  8  9 10
 * 19  6  1  2 11
 * 18  5  4  3 12
 * 17 16 15 14 13
 * </pre>
 * It can be verified that the sum of the numbers on the diagonals is 101.<br/>
 * <br/>
 * <b>What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral 
 * formed in the same way?</b><br/>
 * <br/>
 */
object Problem028 {
  
  def getCornersSum(shell: Int): Int = {
    val delta = shell - 1

    val corner4 = shell * shell
    val corner3 = corner4 - delta
    val corner2 = corner3 - delta
    val corner1 = corner2 - delta
    
    val sum = corner1 + corner2 + corner3 + corner4
    
//    println("Shell %4d: %11d + %11d + %11d + %11d  = %,12d".format(shell, corner1, corner2, corner3, corner4, sum))
    
    sum
  }
  
  def getSumByFormulae(n: Int): Int = 4 * n * n - 6 * n + 6
  
  def evaluate(size: Int, f: Int => Int): (Int, Long) = {
    val t0 = System.currentTimeMillis
    val sum = (3 to size by 2).foldLeft(1)((sum, shell) => sum + f(shell))
    val deltaT = System.currentTimeMillis - t0
    
    (sum, deltaT)
  }
  
  def main(args : Array[String]) : Unit = {
    val size = 9001
    
    println(evaluate(size, getCornersSum))
    println(evaluate(size, getSumByFormulae))
  }
}
