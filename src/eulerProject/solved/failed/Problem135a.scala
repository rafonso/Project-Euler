package eulerProject.solved.failed


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
object Problem135a {
  
  type ZDelta = (Long, Int)
  
  def getSolutions(maxN: Int, numOfSolutions: Int): Int = {
    
    def verifyDelta(n: Int, delta: Int, deltaMax: Int, solutions: List[ZDelta]): Option[List[ZDelta]] = {
      if(delta <= deltaMax) {
        val optSqrt = Utils.getSqrt(4 * delta * delta - n).right.toOption
        if(optSqrt.isDefined && (optSqrt.get > 0)) {
          val zMinus = delta - optSqrt.get
          val zPlus  = delta + optSqrt.get
          if((zMinus > 0) && (solutions.size + 2 <= numOfSolutions)) {
            verifyDelta(n, delta + 1, deltaMax, (zMinus, delta) :: (zPlus, delta) :: solutions)
          } else if((zMinus > 0) && (solutions.size + 2 > numOfSolutions)) {
            None
          } else if(solutions.size + 1 <= numOfSolutions) {
            verifyDelta(n, delta + 1, deltaMax, (zPlus, delta) :: solutions)
          } else {
            None
          }
        } else {
          verifyDelta(n, delta + 1, deltaMax, solutions)
        }
      } else if(solutions.size == numOfSolutions) {
        Some(solutions)
      } else {
        None
      }
    }
    
    def printSolutions(n: Int, solutions: List[ZDelta]) = {
      
      def formatSolution(solution: ZDelta) = "(%,9d, %,9d, %,9d) ".format(solution._1, (solution._1 + solution._2), (solution._1 + 2 * solution._2)) 
      
      print("sol(%,10d) = ".format(n))
      solutions.foreach(solution => print(formatSolution(solution)))
      println
    }
    
    def isValidN(n: Int): Boolean = {
        if(n % 10000 == 0) println("%,10d".format(n))
        val deltaMin = Math.sqrt(n) / 2
        val deltaMax = ((Math.sqrt(n.toLong * (4 * n + 3)) - n) / 3).toInt
        val solutions = verifyDelta(n, deltaMin, deltaMax, Nil)
        if(solutions.isDefined) printSolutions(n, solutions.get)
        solutions.isDefined
    }
    
    (1 to maxN).filter(n => isValidN(n)).size
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 100
    val solutions = 2
    
    val t0 = System.currentTimeMillis
    val result = getSolutions(max, solutions)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
