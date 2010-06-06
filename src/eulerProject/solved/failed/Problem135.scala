package eulerProject.solved.failed

/**
 * Problem 135: Determining the number of solutions of the equation 
 * x<sup>2</sup> - y<sup>2</sup> - z<sup>2</sup> = n.<br>
 * 29 December 2006<br>
 * <br>
 * Given the positive integers, x, y, and z, are consecutive terms of an 
 * arithmetic progression, the least value of the positive integer, n, 
 * for which the equation, x<sup>2</sup> - y<sup>2</sup> - z<sup>2</sup> = n, 
 * has exactly two solutions is n = 27:<br>
 * <br>
 * 34<sup>2</sup> - 27<sup>2</sup> - 20<sup>2</sup> = 12<sup>2</sup> - 9<sup>2</sup> - 6<sup>2</sup> = 27<br>
 * <br>
 * It turns out that n = 1155 is the least value which has exactly ten solutions.<br>
 * <br>
 * <b>How many values of n less than one million have exactly ten distinct solutions?</b><br>
 * <br>
 */
object Problem135 {
  
  def getValidsX(n: Int, delta: Int): Set[Int] = {
    val square = 4 * delta * delta - n
    if(square == 0) Set(3 * delta)
    else {
      val root = Math.sqrt(square)
      if(root * root == square) {
        val xPlus = 3 * delta + root
        val xMinus = 3 * delta - root
        if(xMinus > 2 * delta) Set(xMinus, xPlus)
        else Set(xPlus)
      } else Set()
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val n = 1155
    var delta = Math.sqrt(n) / 2
    val solutions = new scala.collection.mutable.HashSet[Int]
    
    while(solutions.size < 10) {
      val deltaSolutions = getValidsX(n, delta)
      if(deltaSolutions.size > 0) {
        solutions ++ deltaSolutions
        println(delta + " => " + deltaSolutions)
      } 
      delta += 1
    }
    
    println(solutions)
  }
}
/*
 * x^2 - y^2 - z^2 = n
 * y = x - delta
 * z = x - 2 * delta
 * 
 * x^2 - (x - delta)^2 - (x - 2 * delta)^2 = n
 * 
 * x = 3 * delta +- sqrt(4 * delta^2 - n)
 * 
 * 4 * delta^2 - n > 0 <---> delta^2 > n / 4 <---> delta_min = sqrt(n) / 2
 * 
 * Set(40, 50, 52, 74, 100, 134, 208, 482, 290, 1444)
*/