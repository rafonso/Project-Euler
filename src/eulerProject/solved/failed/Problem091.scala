package eulerProject.solved.failed

/**
 * Problem 91: Find the number of right angle triangles in the quadrant.<br/>
 * 18 March 2005<br/>
 * <br/>
 * The points P (x<sub>1</sub>, y<sub>1</sub>) and 
 * Q (x<sub>2</sub>, y<sub>2</sub>) are plotted at integer co-ordinates and 
 * are joined to the origin, O(0,0), to form Triangle(OPQ).<br/>
 * <br/>
 * <div style="text-align: center">
 * <img src="http://projecteuler.net/project/images/p_091_1.gif"/>
 * </div>
 * <br/>
 * There are exactly fourteen triangles containing a right angle that can be 
 * formed when each co-ordinate lies between 0 and 2 inclusive; that is,
 * 0 <= x<sub>1</sub>, y<sub>1</sub>, x<sub>2</sub>, y<sub>2</sub> <= 2.<br/>
 * <br/>
 * <div style="text-align: center">
 * <img src="http://projecteuler.net/project/images/p_091_2.gif"/>
 * </div>
 * <br/>
 * <b>Given that 0 <= x<sub>1</sub>, y<sub>1</sub>, x<sub>2</sub>, 
 * y<sub>2</sub> <= 50, how many right triangles can be formed?</b><br/>
 * <br/>
 */
object Problem091 {
  import _root_.eulerProject.Utils._
    
  def getValidPoints(x0: Int, y0: Int, max: Int): Seq[(Int, Int)] = x0 match {
    case 0 => Nil
    case _ if(y0 == 0) => (1 to max).map((x0, _)).toList
    case _ => {
      val r2 = x0 * x0 + y0 * y0
      
      def evaluateX(x: Int): Option[(Int, Int)] = /%(- x0 * x + r2, y0) match {
        case (y, 0) => Some(x, y)
        case _ => None
      }
      
      val xInitial = Math.max(0  , r2 / x0 - max)
      val xFinal   = Math.min(max, r2 / x0      )
      
      log("(%2d, %2d) => (%2d, %2d)".format(x0, y0, xInitial, xFinal))
      
      (xInitial to xFinal)
        .filter(_ != x0)
        .map(evaluateX(_))
        .filter(_.isDefined)
        .map(_.get).toList
    }
  }
  
  def getValidTriangles(max: Int): Int = {
    
    def evaluateX0(x0: Int, y0: Int): Int = {
      val points = getValidPoints(x0, y0, max)
      log("(%2d, %2d) => %s".format(x0, y0, points))
      points.size
    }
    
    (0 to max)
      .foldLeft(0)(
        (sum, y0) => sum + (0 to max).foldLeft(0)(_ + evaluateX0(_, y0))
      )
  }
  
  
  def main(args : Array[String]) : Unit = {
    val max = 2
    
    val t0 = System.currentTimeMillis
    val result = getValidTriangles(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
