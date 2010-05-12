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
object Problem135b {
  
  class Solution(z: Int, delta: Int, n: Int) {
    
    val y = z + delta 
    
    val x = y + delta
    
    override def toString =  "(%,9d, %,9d, %,9d) ".format(x, y, z)
  }
  
  def printSolutions(n: Int, solutions: List[Solution]) = {
    print("sol(%,10d) = ".format(n))
    solutions.foreach(print(_))
    println
  }
  
  def verifyDelta(n: Int, delta: Int, deltaMax: Int, numMaxSolutions: Int, solutions: List[Solution]): Option[List[Solution]] = {
    
    def getSolutionForRoot0 = {
      if(solutions.size + 1 <= numMaxSolutions) Some(new Solution(delta, delta, n) :: solutions)
      else None
    }
    
    def getSolutionForRootGreaterThanDelta(root: Int) = {
      if(solutions.size + 1 <= numMaxSolutions) Some(new Solution(delta + root, delta, n) :: solutions)
      else None 
    }
    
    def getSolutionForRootLesserThanDelta(root: Int) = {
      if(solutions.size + 2 <= numMaxSolutions) Some(new Solution(delta - root, delta, n) :: new Solution(delta + root, delta, n) :: solutions)
      else None
    }
    
    if(delta <= deltaMax) {
      val sqrt = Utils.getSqrt(4 * delta * delta - n).right.toOption
      if(sqrt.isDefined) {
        val nextSolutions = if(sqrt.get == 0) {
          getSolutionForRoot0
        } else if(sqrt.get < delta) {
          getSolutionForRootLesserThanDelta(sqrt.get.intValue)
        } else {
          getSolutionForRootGreaterThanDelta(sqrt.get.intValue)
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
    val deltaMin = Math.sqrt(n) / 2
    val deltaMax = ((Math.sqrt(n.toLong * (4 * n + 3)) - n) / 3).toInt
    val solutions = verifyDelta(n, deltaMin, deltaMax, numMaxSolutions, Nil)
    if(solutions.isDefined) printSolutions(n, solutions.get)
    solutions.isDefined
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 10000
    val numMaxSolutions = 10
    
    val t0 = System.currentTimeMillis
    val result = (1 until max).filter(isValidN(_, numMaxSolutions)).size
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
