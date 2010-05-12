package eulerProject

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
object Problem276a {
  
  type Triangle = (Long, Long, Long)
  
  def getCommonDivisor(primes: List[Long], numbers: Seq[Long]): Long = {
    
    val limit = numbers(0) / 2
    
    def evaluatePrime(currentPrimes: List[Long]): Long = currentPrimes match {
      case p :: others if(p > limit) => 1
      case p :: others if(numbers.forall(_ % p == 0)) => p
      case p :: others => evaluatePrime(others)
      case Nil => error("I don't have more primes for evaluate. Numbers: " + numbers.toString)
    }
    
    evaluatePrime(primes)
  }
  
  def getTrianglesForA(a: Long, limit: Long, primes: List[Long]): List[Triangle] = {
    
    def getTrianglesForB(b: Long, cMax: Long, c: Long, triangles: List[Triangle]): List[Triangle] = {
      if(c < cMax) {
        val divisor = getCommonDivisor(primes, Array(a, b, c))
//        println("divisor(" + a + ", " + b + ", " + c + ") = " + divisor)
        val nextTriangles = if(divisor == 1) (a, b, c) :: triangles else triangles
        getTrianglesForB(b, cMax, c + 1, nextTriangles)
      } else {
        triangles.reverse
      }
    }
    
    val bMax = (limit - a) / 2
    val trianglesForA = (a.toInt to bMax.toInt).flatMap(b => getTrianglesForB(b, bMax, b, Nil)).toList
//    println("Triangles for %,9d (%,9d): %s".format(a, trianglesForA.size, trianglesForA ))
    println("Triangles for %,9d: %,9d".format(a, trianglesForA.size))
    trianglesForA
  }
  
  def getTrianglesForA1(a: Long, limit: Long, primes: List[Long]): Int = {
    
    val bMax = ((limit - a) / 2).toInt
    
    def getTrianglesForB(b: Long, cMax: Long, c: Long, size: Int): Int= {
      if(c < cMax) {
        val divisor = getCommonDivisor(primes, Array(a, b, c))
        println("divisor(" + a + ", " + b + ", " + c + ") = " + divisor)
        val add = if(divisor == 1) 1 else 0
        getTrianglesForB(b, cMax, c + 1, size + add)
      } else {
        size
      }
    }
    
    def cHasPrimitive(b: Long, c: Long): Int = if(getCommonDivisor(primes, Array(a, b, c)) == 1) 1 else 0
    
    def getQuantityForC(b: Int): Int = (b to bMax).foldLeft(0)(_ + cHasPrimitive(b, _))
    
    val quantityForA = (a.toInt to bMax).foldLeft(0)((quantityForB, b) => quantityForB + getQuantityForC(b))
//    println("Triangles for %,9d (%,9d): %s".format(a, trianglesForA.size, trianglesForA ))
    println("Triangles for %,9d: %,9d".format(a, quantityForA))
    quantityForA 
  }
  
  def main(args : Array[String]) : Unit = {
    val limit = 1000
    
    getTrianglesForA1(10, limit, Utils.getPrimesUntil(limit))
    
  }
}
