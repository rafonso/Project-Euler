package eulerProject.solved

/**
 * EULER: SOLVED
 */
object Problem006 {
  
  // Gauss Formulae
  //def getSum(n: Int, f: (Int => Int)) = n * (1 + f(n)) / 2
  
  def square(n: Int): Int = n * n
  
  def sumSquares(accumulated: Int, current: Int) = accumulated + square(current) 
  
  def sum(n: Int): Int = (1 to n).foldLeft(0)(_ + _)
  
  def main(args : Array[String]) : Unit = {
    val n = Integer.parseInt(args(0))
    
    val t0 = System.currentTimeMillis
    val range = (1 to n)
      //getSum(n, (i => i))
    val squareOfSum = square(sum(n))
    val sumOfSquares = range.foldLeft(0)(sumSquares)
    val difference = squareOfSum - sumOfSquares 
    val deltaT = System.currentTimeMillis - t0
    
    printf("[Sum(%d) * Sum(%d)] - Sum(%d * %d) = %d - %d = %d%n", n, n, n, n, squareOfSum, sumOfSquares, difference)
    println("Total Time: " + deltaT + " ms")
  }
}
