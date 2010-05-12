package eulerProject.solved

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
object Problem091a {
  
  case class Point(x: Int, y: Int) {
    
    def quadraticDistance(other: Point) = {
      val deltaX = this.x - other.x
      val deltaY = this.y - other.y
      
      deltaX * deltaX + deltaY * deltaY
    }
    
    lazy val quadraticRadius = this.x * this.x + this.y * this.y
    
    override def toString = "(%2d, %2d)".format(this.x, this.y)
  }
  
  def getTrianglesForPoint(p: Point, max: Int): List[Point] = {
    
    def showPoints(pts: List[Point]) {
      if(!pts.isEmpty) {
        print(p + " => ")
        pts.reverse.foreach(pt => print(pt + " "))
        println
      }
    }
    
    def getForRow(x: Int, y: Int, xs: List[Point]): List[Point] = {
      if(x <= max) {
        val pxy = Point(x, y)
        val quadraticSides = List(p.quadraticRadius, pxy.quadraticRadius, p.quadraticDistance(pxy)).sort(_ < _)
        if(quadraticSides(0) + quadraticSides(1) == quadraticSides(2)) getForRow(x + 1, y, pxy :: xs)
        else getForRow(x + 1, y, xs)
      } else {
        xs.reverse
      }
    }
    
    val points =  getForRow(p.x + 1, p.y, Nil) ::: ((p.y + 1) to max).flatMap(getForRow(0, _, Nil)).toList
    showPoints(points.reverse)
    points
  } 
  
  def getForY(x0: Int, y: Int, max: Int) = (x0 to max)
    .foldLeft(0)((sum, x) => sum + getTrianglesForPoint(Point(x, y), max).size)
  
  def main(args : Array[String]) : Unit = {
    val max = 50
    
    val t0 = System.currentTimeMillis
    val result = getForY(1, 0, max) + (1 to max).foldLeft(0)((sum, y) => sum + getForY(0, y, max))
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
