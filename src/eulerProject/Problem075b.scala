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
object Problem075b {
  
  import Utils._
  
  case class  FoundState(qty: Int)
  case object FoundNone extends FoundState(0)
  case object FoundOne  extends FoundState(1)
  case object FoundMore extends FoundState(2)
  
  def verifyTrianglesFor(perimeter: Int): Boolean = {
    
    def evaluateAB(a: Long, b: Long, bMax: Long, state: FoundState): FoundState = {
      if(b <= bMax) {
        val cSquare = a * a + b * b
        val c = perimeter - a - b
        if(cSquare == c * c) {
          state match {
            case FoundNone => {
              println("p = %,9d => (%,9d, %,9d, %,9d)".format(perimeter, a, b, c))
              evaluateAB(a, b + 1, bMax, FoundOne)
            }
            case FoundOne =>  FoundMore
            case FoundMore => error("Achei (%,9d, %,9d, %,9d), mas já havia um triangulo anterior para %,9d".format(a, b, c, perimeter))
          }
        } else {
          evaluateAB(a, b + 1, bMax, state)
        }
      } else {
        state
      }
    }
    
    def evaluateA(a: Long, aMax: Long, state: FoundState): FoundState = {
      if(a < aMax) {
        val nextState = evaluateAB(a, a, (perimeter - a) / 2, state) 
        nextState match {
          case FoundNone | FoundOne => evaluateA(a + 1, aMax, nextState)
          case FoundMore => FoundMore
        } 
      } else {
        state
      }
    }
    
    evaluateA(1, perimeter / 3, FoundNone) == FoundOne
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 15000
    
    val t0 = System.currentTimeMillis
    val result = verifyTrianglesFor(1500000)
      //(1 to max).filter(perimeter => verifyTrianglesFor(perimeter)).size
    val deltaT = System.currentTimeMillis - t0
    
    println("=================================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
