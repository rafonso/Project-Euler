package eulerProject

object Problem251b {
  
  import Utils._
  
  def evaluateK(k: Int): Int = {
    
    def evaluateD(a: Int, d: Int, bNumerator: Int): Boolean = {
      bNumerator /% (8 * d) match {
        case (b, 0) => {
          val c = Math.sqrt(d)
          log("a = %,10d, b = %,10d, c = %,10d, k = %,10d, d = %,10d".format(a, b, c, k, d))
          true
        }
        case _ => false
      }
    }
    
    (3 * k * k + 1) /% 8 match {
      case (a, 0) => {
        val bNumerator = k * (k * k + 3)
        val dMax = bNumerator / 8
        (1 to dMax).filter(d => evaluateD(a, d, bNumerator)).size
      }
      case _ => 0
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
    val kMax = (2 * java.lang.Math.cbrt(max)).toInt
    log(kMax)
    val t0 = System.currentTimeMillis
    val result = (1 to kMax).foldLeft(0)(_ + evaluateK(_))
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
