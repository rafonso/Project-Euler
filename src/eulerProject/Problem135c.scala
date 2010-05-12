package eulerProject

/**
 * Problem 135: Determining the number of solutions of the equation 
 * x<sup>2</sup> - y<sup>2</sup> - z<sup>2</sup> = n.<br>
 * 29 December 2006<br>
 * <br>
 * Given the positive integers, x, y, and z, are consecutive terms of an 
 * arithmetic progression, the least value of the positive integer, n, 
 * for which the equation, x<sup>2</sup> - y<sup>2</sup> - z<sup>2</sup> = n, 
 * has exactly two solutions is n = 27:<br>
 * <br>
 * 34<sup>2</sup> - 27<sup>2</sup> - 20<sup>2</sup> = 12<sup>2</sup> - 9<sup>2</sup> - 6<sup>2</sup> = 27<br>
 * <br>
 * It turns out that n = 1155 is the least value which has exactly ten solutions.<br>
 * <br>
 * <b>How many values of n less than one million have exactly ten distinct solutions?</b><br>
 * <br>
 */
object Problem135c {
  
  class Solution(x: Int, delta: Int, n: Int) {
    
    lazy val y = x - delta 
    
    lazy val z = x - 2 * delta
    
    override def toString =  "(%,9d, %,9d, %,9d) ".format(x, y, z)
  }
  
  def printSolutionsForN(n: Int, solutions: List[Solution]) = {
    print("sol(%,10d) = ".format(n))
    solutions.foreach(print(_))
    println
  }
  
  /*
   * x^2 - y^2 - z^2 = n, y = x - d, z = x - 2 * d
   *  --> x^2 - 6 * d * x + (5 * d ^2 + n) = 0
   * <--> x = 3 * d +- sqrt(4 * d ^2 - n)
   * 
   * 4 * d ^ 2 - n >= 0 <--> d >= sqrt(n) / sqrt(2)
   * 
   * z >= 0 <--> x - 2 * d >= 0 <--> 3 * d +- sqrt(4 * d ^2 - n) - 2 * d >= 0
   * <--> +- sqrt(4 * d ^2 - n) >= - d <--> 4 * d ^2 - n >= d ^2 
   * <--> 3 * d^2 >= n <--> d >= sqrt(n) / sqrt(3)
   * 
   * How 1 / sqrt(3) > 1 / sqrt(2) --> dMin = sqrt(n) / sqrt(3) = sqrt(3 * n) / 3
   */
  def verifyDelta(n: Int, delta: Int, deltaMax: Int, numMaxSolutions: Int, solutions: List[Solution]): Option[List[Solution]] = {
  
    def getSolutionForRoot0 = {
      if(solutions.size + 1 <= numMaxSolutions) Some(new Solution(3 * delta, delta, n) :: solutions)
      else None
    }
    
    def getSolutionForRoot(root: Int) = {
      val aux = 3 * delta
      val solutionMinus = new Solution(aux - root, delta, n)
      val solutionPlus  = new Solution(aux + root, delta, n)
      
      if(solutionMinus.z > 0) {
        if(solutions.size + 2 <= numMaxSolutions) Some(solutionPlus :: solutionMinus :: solutions)
        else None
      } else if(solutions.size + 1 <= numMaxSolutions) {
        Some(solutionPlus :: solutions)
      } else {
        None
      } 
    }
    
    if(delta <= deltaMax) {
      val sqrt = Utils.getSqrt(4 * delta * delta - n).right.toOption
      if(sqrt.isDefined) {
        val nextSolutions = if(sqrt.get == 0) {
          getSolutionForRoot0
        } else {
          getSolutionForRoot(sqrt.get.intValue)
        }
        
        if(nextSolutions.isDefined) verifyDelta(n, delta + 1, deltaMax, numMaxSolutions, nextSolutions.get)
        else None
      } else {
        verifyDelta(n, delta + 1, deltaMax, numMaxSolutions, solutions)
      }
    } else if(solutions.size == numMaxSolutions) {
      Some(solutions)
    } else {
      None
    }
  }
  
  def isValidN(n: Int, numMaxSolutions: Int): Boolean = {
    if(n % 10000 == 0) println("%,10d".format(n))
    val deltaMin = Math.sqrt(3 * n) / 3
    val deltaMax = n  + 1
    val solutions = verifyDelta(n, deltaMin, deltaMax, numMaxSolutions, Nil)
    if(solutions.isDefined) printSolutionsForN(n, solutions.get)
    solutions.isDefined
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 10000
    val numMaxSolutions = 10
    
    val t0 = System.currentTimeMillis
    val result = 
//      verifyDelta(27, 3, 28, 2, Nil)
      (1 until max).filter(isValidN(_, numMaxSolutions)).size
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
