package eulerProject.solved

/**
 * EULER: SOLVED
 */
object Problem120b {
  
  def getResult(a: Int) = if(a % 2 == 0) a * (a - 2) else a * (a - 1)
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val sum = (3 to 1000).foldLeft(0)(_ + getResult(_))
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(sum)
    println("Time = " + deltaT + " ms")
  }
}
