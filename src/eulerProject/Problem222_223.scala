package eulerProject

/**
 * Almost right-angled triangles.<br/>
 * <br/>
 * <hr>
 * Problem 223<br/>
 * 26 December 2008<br/>
 * <br/>
 * Let us call an integer sided triangle with sides a <= b <= c barely acute 
 * if the sides satisfy a<sup>2</sup> + b<sup>2</sup>= c<sup>2</sup> + 1.<br/>
 * <br/>
 * <b>How many barely acute triangles are there with 
 * perimeter <= 25,000,000?</b><br/>
 * <br/>
 * <hr>
 * Problem 224<br/>
 * 26 December 2008<br/>
 * <br/>
 * Let us call an integer sided triangle with sides a <= b <= c barely obtuse 
 * if the sides satisfy a<sup>2</sup> + b<sup>2</sup> = c<sup>2</sup> - 1.<br/>
 * <br/>
 * <b>How many barely obtuse triangles are there with 
 * perimeter <= 75,000,000?</b><br/>
 * 
 */
object Problem222_223 {
  
  import Utils._
  
  def getQuantityTriangles(maxPerimeter: Long, delta: Int): Int = {
    
    def showTriangle(a: Long, b: Long, c: Long) {
      val p = a + b + c
      log("a = %,10d, b = %,10d, c = %,10d, perimeter = %,10d".format(a, b, c, p))
    }
    
    def evaluateAB(a: Long, b: Long, bMax: Long, cMax: Long, qtyAB: Int): Int = {
      if(b > bMax) {
        qtyAB
      } else getSqrt(a * a + b * b - delta) match {
        case Right(c) if(c > cMax) => qtyAB
        case Left(c)  if(c > cMax) => qtyAB
        case Right(c) => {
          showTriangle(a, b, c)
          val ab1 = a + b + 1
          evaluateAB(a, b + 1, bMax, Math.min(ab1, maxPerimeter - ab1), qtyAB + 1)
        }
        case Left(c) => {
          val ab1 = a + b + 1
          evaluateAB(a, b + 1, bMax, Math.min(ab1, maxPerimeter - ab1), qtyAB)
        }
      }
    }
    
    val aMax = maxPerimeter / 3
    (1 to aMax.toInt).foldLeft(0)((sum, a) => sum + evaluateAB(a, a, ((maxPerimeter - a) / 2), Math.min(2 * a, maxPerimeter - 2 * a), 0))
  }
  
  def main(args : Array[String]) : Unit = {
    val maxPerimeter = 25000000L
    val delta = -1
    
    val t0 = System.currentTimeMillis
    val result = getQuantityTriangles(maxPerimeter, delta)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
