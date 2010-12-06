package eulerProject;

/**
 * Problem 276: Primitive Triangles<br/>
 * 29 January 2010<br/>
 * <br/>
 * Consider the triangles with integer sides a, b and c with a <= b <= c.<br/>
 * <br/>
 * An integer sided triangle (a,b,c) is called primitive if gcd(a,b,c)=1.<br/>
 * <br/>
 * <b>How many primitive integer sided triangles exist with a perimeter not 
 * exceeding 10 000 000?</b><br/>
 * 
 */
object Problem276b {
  
  import Utils._
  
  def evaluateAB(a: Long, b: Long, bMax: Long, primes: List[Long], qty: Int): Int = {
    if(b > bMax) {
      qty
    } else if(primes.contains(b)){
      0
    } else {
      0
    }
  }
  
  def evaluateA(a: Long, pMax: Long, primes: List[Long]): Int = {
    
    
    
    val bMax = (pMax - a) / 2
    0
  }
  
  def main(args : Array[String]) : Unit = {
    /*
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = max
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
    */
    
    val dividend = 129L
    val divisor = 34
    println(dividend /% divisor)
  }
}
