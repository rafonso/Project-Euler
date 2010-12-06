package eulerProject.solved.failed

/**
 * Problem 142: Perfect Square Collection<br>
 * 24 February 2007<br>
 * <br>
 * <b>Find the smallest x + y + z with integers x > y > z > 0 such that x + y, 
 * x - y, x + z, x - z, y + z, y - z are all perfect squares.</b><br>
 * 
 */
object Problem142 {
  
  def evaluateX(x: Long): (Long, Long, Long) = {
    
    def isPerfectSquare(n: Long)= eulerProject.Utils.getSqrt(n).isRight
    
    def evaluateZ(y: Long, z: Long): Option[(Long, Long, Long)] = {
      if(y < z) {
        if(isPerfectSquare(x + z) && isPerfectSquare(x - z) && isPerfectSquare(y + z) && isPerfectSquare(y - z)) Some((x, y, z))
        else evaluateZ(y, z + 1)
      } else {
        None
      }
    }
    
    def evaluateY(y: Long): Option[(Long, Long, Long)] = {
      if(y < x) {
        if(isPerfectSquare(x + y) && isPerfectSquare(x - y)) {
          val now = new java.util.Date
          println("[%tT.%tL] (%,9d, %,9d)".format(now, now, x, y))
          val resultForY = evaluateZ(y, 1)
          if(resultForY.isDefined) resultForY
          else evaluateY(y + 1)
        } else {
          evaluateY(y + 1)
        }
      } else {
        None
      }
    }
    
    val resultForX = evaluateY(1)
    if(resultForX.isDefined) resultForX.get
    else evaluateX(x + 1)
  } 
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = evaluateX(1)
    val sum = result._1 + result._2 + result._3
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println("Result: " + result)
    println("Sum: " + sum)
    println("Time = " + deltaT + " ms")
  }
}
