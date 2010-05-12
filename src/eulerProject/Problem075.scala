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
object Problem075 {
  
  def isValidPerimeter(p: Long): Boolean = {
    
    def bNumerator(a: Long) = p * (2 * a - p)
    def bDenominator(a: Long) = 2 * (a - p)
    
    def getResultsForA(a: Long, found: Boolean): Boolean = {
      if(a >= p /2) found
      else {
        val bNum = bNumerator(a)
        val bDen = bDenominator(a)
        val b = bNum / bDen 
        if(b < a) found
        else if(bNum % bDen == 0) {
//          println("(" + a + ", " + (bNum / bDen) + ", " + p + ") -> " + found)
          if(found) false
          else getResultsForA(a + 1, true)
        } else   
          getResultsForA(a + 1, found)
      }
    }
    
    getResultsForA(1L, false)
  }
  
  def getSolutions(max: Int): Long = {
    
    def print(p: Long, qty: Long) = if(p % 1000 == 0) printf("qty(%,9d) = %,6d%n", p, qty)

    def eval2(sum: Int, p: Long): Int = {
      print(p - 1, sum)
      if(isValidPerimeter(p)) sum + 1 else sum
    }
    
    (3 to max).foldLeft(0)(eval2(_, _))
  }
  
  def main(args : Array[String]) : Unit = {
    val n = 1500000
    
    val t0 = System.currentTimeMillis
    val result = getSolutions(n)
    val deltaT = System.currentTimeMillis - t0
    
    println("=================================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
