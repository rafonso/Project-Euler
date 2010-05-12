package eulerProject

/**
 * Problem 15: Starting in the top left corner in a 20 by 20 grid, how many 
 * routes are there to the bottom right corner?<br>
 * 19 April 2002<br>
 * <br>
 * Starting in the top left corner of a 2×2 grid, there are 6 routes 
 * (without backtracking) to the bottom right corner.<br>
 * <br>
 * How many routes are there through a 20×20 grid?<br>
 */
object Problem015 {
  
  def getRoutes(size: Int): Long = {
    
    def walk(currentRoute: List[(Int, Int)], routes: Long): Long = currentRoute match {
      case (row, col) :: priors if((row == size) && (col == size)) => {
//        println(currentRoute.reverse)
        routes + 1
      }
      case (row, col) :: priors if((row == size) && (col <  size)) => walk((size, col + 1) :: currentRoute, routes)
      case (row, col) :: priors if((row <  size) && (col == size)) => walk((row + 1, size) :: currentRoute, routes)
      case (row, col) :: priors if((row <  size) && (col <  size)) => {
        val routesForCol = walk((row + 1, col) :: currentRoute, routes)
        walk((row, col + 1) :: currentRoute, routesForCol)
      }
      case _ => error("routes: " + routes + ", current route: " + currentRoute)
    }
    
    walk(List((0, 0)), 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val size = 20
    
    val t0 = System.currentTimeMillis
    val result = getRoutes(size)
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================================================")
    println("Routes for " + size + " = " + result)
    println("Time = " + deltaT + " ms")
  }
}
