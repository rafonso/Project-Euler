package eulerProject.solved

/**
 * Problem 39: If p is the perimeter of a right angle triangle, {a, b, c}, 
 * which value, for p <= 1000, has the most solutions?<br>
 * 14 March 2003<br>
 * <br>
 * If p is the perimeter of a right angle triangle with integral length sides, 
 * {a,b,c}, there are exactly three solutions for p = 120.<br>
 * <br>
 * {20,48,52}, {24,45,51}, {30,40,50}<br>
 * <br>
 * <b>For which value of p <= 1000, is the number of solutions 
 * maximised?</b><br>
 * <br>
 * EULER: SOLVED
 */
object Problem039 {
  
  def getSolutionsForPerimeter(p: Int): List[(Int, Int, Int)] = {
    
    def getResultsForAB(a: Int, b: Int, results: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
      val c = p - a - b
//      printf("(%d, %d, %d)%n", a, b, c)
      if(b >= c) results
      else if((a * a + b * b) == (c * c)) getResultsForAB(a, b + 1, (a, b, c) :: results)
      else getResultsForAB(a, b + 1, results)
    }
    
    def getResultsForA(a: Int, results: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
      if(a >= p /2) results
      else getResultsForA(a + 1, results ::: getResultsForAB(a, a + 1, Nil))
    }
    
    getResultsForA(1, Nil)
  }
  
  def getSolutions(minPerimeter: Int, maxPerimeter: Int): Int = {
    
    def eval(p: Int, maxSize: Int, pResult: Int): Int = {
      if(p > maxPerimeter)
        pResult
      else {
        val results = getSolutionsForPerimeter(p)
        if(results.size > 0) println(p + " => " + results)
        if(results.size > maxSize) eval(p + 1, results.size, p)
        else eval(p + 1, maxSize, pResult)
      }
    }
    
    eval(minPerimeter, 0, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val min = 1
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = getSolutions(min, max)
      //getSolutionsForPerimeter(12)
      //getSolutions(min, max)
    val deltaT = System.currentTimeMillis - t0
    
    println("=================================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
  
  def getQuadArea(a: Int, b: Int, c: Int) = (a + b + c) * (- a + b + c) * (a - b + c) * (a + b - c) / 16
}
