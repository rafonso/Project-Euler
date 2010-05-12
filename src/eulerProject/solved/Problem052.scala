package eulerProject.solved

/**
 * Problem 52: Find the smallest positive integer, x, such that 2x, 3x, 4x, 
 * 5x, and 6x, contain the same digits in some order.<br>
 * 12 September 2003<br>
 * <br>
 * It can be seen that the number, 125874, and its double, 251748, contain 
 * exactly the same digits, but in a different order.<br>
 * <br>
 * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, 
 * contain the same digits.<br>
 * EULER: SOLVED
 */
object Problem052 {
  
  def normalize(n: Int): String = new String(n.toString.toArray.toList.sort(_ < _).toArray)
  
  def evaluate(x: Int, max: Int): Boolean = {
    val xString = normalize(x)
    
    def eval(n: Int, i: Int): Boolean =  {
      if(i > max) true
      else if(!normalize(n).equals(xString)) false
      else {
//        print(n + " ")
        eval(n + x, i + 1)
      }
    }
    
//    print("\n" + x + " ")
    eval(x + x, 2)
  }
  
  def getSmallestNumberFor(i: Int, max: Int): Int = 
    if(evaluate(i, max)) i
    else getSmallestNumberFor(i + 1, max)
  
  def main(args : Array[String]) : Unit = {
    val max = 6
    
    val t0 = System.currentTimeMillis
    val result = getSmallestNumberFor(2, max)
    val deltaT = System.currentTimeMillis - t0
    
    println("")
    println("==================")
    println(max + " => " + result)
    println("Time = " + deltaT + " ms")

  }
}
