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
object Problem075a {
  
  import Utils._
  
  type Triangle = (Long, Long, Long)
  
  class CounterMap extends scala.collection.jcl.TreeMap[Long, Int]  {
    
    def add(value: Long) {
      super.put(value, super.getOrElse(value, 0) + 1)
    }
    
  }
  
  def evaluateAB(a: Long, b: Long, max: Long, triangles: List[Triangle]): List[Triangle] = {
    val eitherC = getSqrt(a * a + b * b)
    val c = if(eitherC.isRight) eitherC.right.get else eitherC.left.get.toLong
    val nextTriangles = if(eitherC.isRight) (a, b, c) :: triangles else triangles
    val perimeter = a + b + c
    
    if((perimeter > max) || (a + b < c)) {
      if(!triangles.isEmpty) println(triangles.reverse)
      triangles
    } else {
      evaluateAB(a, b + 1, max, nextTriangles)
    }
  }
  
  def countTriangles(max: Long): Long = {
    
    val counter = new CounterMap
    
    def perimeter(triangle: Triangle) = triangle._1 + triangle._2 + triangle._3
    
    def count(a: Long) = evaluateAB(a, a + 1, max, Nil).foreach(t => counter.add(perimeter(t)))
    
    val limit = max / 3 + 1
    (1 to limit.toInt).foreach(count(_))
    counter.filter(_._2 == 1).map(_._1).foldLeft(0L)(_ + _)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1500000
    // L = a + a + 1 + a + 2 = 3a + 3 <--> aMax = L / 3 + 1
    val limit = max / 3 + 1
    
    val t0 = System.currentTimeMillis
    val result = countTriangles(max)
      //(1 to limit).foldLeft(0L)((sum, a) => sum + evaluateAB(a, a + 1, max, Nil))
    val deltaT = System.currentTimeMillis - t0
    
    println("=================================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
