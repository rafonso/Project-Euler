package eulerProject.solved

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
object Problem135d {
    
  import eulerProject.Utils._
  
  def nHasSolutions(n: Long, maxSolutions: Int): Boolean = {
    
    type DDelta = (Long, Int)
    
    val dMaxForMinusDelta = Math.sqrt(n / 3)
    val dMax = n/2
    
    def nextValues(d: Long, delta: Int, hasRoot: Boolean): DDelta = {
      if(d > dMaxForMinusDelta) (d + 1, 1)
      else if(hasRoot) {
        if(delta == -1) (d, 1)
        else (d + 1, -1)
      } else { 
        (d + 1, -1)
      }
    }
    
    def evaluateD(dDelta: DDelta, zs: Set[Long]): Boolean = {
      if(dDelta._1 >= dMax) (zs.size == maxSolutions)
      else getSqrt(4 * dDelta._1 * dDelta._1 - n) match {
        case Right(root) => {
          val z = dDelta._1 + dDelta._2 * root
          if(z > 0) {
//            log(z + " - " + dDelta._1)
            val nextZs = zs + z
            if(nextZs.size > maxSolutions) false
            else evaluateD(nextValues(dDelta._1, dDelta._2, true), nextZs)
          } else evaluateD(nextValues(dDelta._1, dDelta._2, true), zs)
        }
        case _ => evaluateD(nextValues(dDelta._1, dDelta._2, false), zs)
      }
    }
    
    val dMin = Math.sqrt(n / 4).toLong
    evaluateD((dMin, -1), Set.empty[Long])
  }
  
  def showResult(qty: Int, n: Int): Int = {
//    log("%,9d".format(n))
    qty + 1
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000000
    val qtyMax = 10
    
    val t0 = System.currentTimeMillis
    val result = (1 to max).filter(n => nHasSolutions(n, qtyMax)).foldLeft(0)((qty, n) => showResult(qty, n))
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
