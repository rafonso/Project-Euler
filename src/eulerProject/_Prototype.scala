package eulerProject

/**
 * 
 */
object _Prototype {
  
  import Utils._
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = max
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
