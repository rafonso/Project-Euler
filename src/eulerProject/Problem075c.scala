package eulerProject

/**
 * Problem 75: Find the number of different lengths of wire can that can form 
 * a right angle triangle in only one way.<br>
 * 30 July 2004<br>
 * <br>
 * It turns out that 12 cm is the smallest length of wire that can be bent to 
 * form an integer sided right angle triangle in exactly one way, but there 
 * are many more examples.<br>
 * <br>
 * 12 cm: (3,4,5)<br>
 * 24 cm: (6,8,10)<br>
 * 30 cm: (5,12,13)<br>
 * 36 cm: (9,12,15)<br>
 * 40 cm: (8,15,17)<br>
 * 48 cm: (12,16,20)<br>
 * <br>
 * In contrast, some lengths of wire, like 20 cm, cannot be bent to form an 
 * integer sided right angle triangle, and other lengths allow more than one 
 * solution to be found; for example, using 120 cm it is possible to form 
 * exactly three different integer sided right angle triangles.<br>
 * <br>
 * 120 cm: (30,40,50), (20,48,52), (24,45,51)<br>
 * <br>
 * <b>Given that L is the length of the wire, for how many values of 
 * L <= 1,500,000 can exactly one integer sided right angle triangle be 
 * formed?</b><br>
 * <br>
 * Note: This problem has been changed recently, please check that you are 
 * using the right parameters.<br>
 * <br>
 */
object Problem075c {
  
  import Utils._
  
  type Triangle = (Long, Long, Long, Long)
  
  def getTriangleFrom(l: Long): Option[Triangle] = {
    
    def evalA(a: Long, aMax: Long, foundTriangle: Option[Triangle]): Option[Triangle] = {
      if(a > aMax) {
        foundTriangle
      } else {
        val den = 2 * (l - a)
        /%(l * (l - 2 * a), den) match {
          case(b, 0) if(b >= a) => /%(l * l - 2 * a * l + 2 * a * a, den) match {
            case (c, 0) if( foundTriangle.isDefined) => None
            case (c, 0) if(!foundTriangle.isDefined) => evalA(a + 1, aMax, Some((l, a, b, c)))
            case _ => evalA(a + 1, aMax, foundTriangle)
          }
          case (b, _) if(b < a) => foundTriangle
          case _ => evalA(a + 1, aMax, foundTriangle)
        }
      }
    }
    
    evalA(1, l / 2, None)
  }
  
  def filter(optTriangle: Option[Triangle]): Boolean = {
    if(optTriangle.isDefined) {
      val triangle: Triangle = optTriangle.get
      log("%,9d = %,7d + %,7d + %,7d".format(triangle._1, triangle._2, triangle._3, triangle._4))
      true
    } else {
      false
    }
  }
  
  
  def main(args : Array[String]) : Unit = {
    val max = 1500000
    
    val t0 = System.currentTimeMillis
    val result = (3 to max)
      .map(getTriangleFrom(_))
      .filter(filter(_))
      .size
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
