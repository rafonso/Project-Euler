package eulerProject

/**
 * Problem 273: Sum of Squares<br/>
 * 09 January 2010<br/>
 * <br/>
 * Consider equations of the form: a<sup>2</sup> + b<sup>2</sup> = N, 
 * 0 <= a <= b, a, b and N integer.<br/>
 * <br/>
 * For N=65 there are two solutions:<br/>
 * <br/>
 * a = 1, b = 8 and a = 4, b = 7.<br/>
 * <br/>
 * We call S(N) the sum of the values of a of all solutions of 
 * a<sup>2</sup> + b<sup>2</sup> = N, 0 <= a <= b, a, b and N integer.<br/>
 * <br/>
 * Thus S(65) = 1 + 4 = 5.<br/>
 * <br/>
 * <b>Find Sum(S(N)), for all squarefree N only divisible by primes of the 
 * form 4k+1 with 4k+1 < 150.</b><br/>
 * <br/>
 */
object Problem273a {
  
  import Utils._
  
  val primes: Array[Long] = Array(149, 137, 113, 109, 101, 97, 89, 73, 61, 53, 41, 37, 29, 17, 13, 5) 
  //, 37, 41, 53, 61, 73, 89, 97, 101, 109, 113, 137, 149)
  // 149, 137, 113, 109, 101, 97, 89, 73, 61, 53, 41, 37, 29, 17, 13, 5
  val primeSizes = Math.pow(2, primes.size).toInt
  
  def evaluateN(n: Long): Long = {
    
    def evaluateA(a: Long, aMax: Long, ab: List[(Long, Long)]): Long = {
      if(a <= aMax) {
        getSqrt(n - a * a) match {
          case Right(b) => evaluateA(a + 1, aMax, (a, b) :: ab)
          case _ => evaluateA(a + 1, aMax, ab)
        }
      } else {
        val sum = ab.foldLeft(0L)(_ + _._1)
        log("S(%,10d) = %,10d => %s".format(n, sum, ab))
        sum
      }
    }
    
    evaluateA(1, Math.sqrt(n.toInt / 2).toLong, Nil)
  }
  
  def getN(index: Int) = {
    val booleans = intToBoolList(index, primes.size)
    (0 until booleans.size).foldLeft(BigInt(1))((prod, i) => if(booleans(i)) prod * primes(i) else prod)
  }

  def main(args : Array[String]) : Unit = {
    val max = 85
    
    val t0 = System.currentTimeMillis
    val result = evaluateN(85)
//    (1 until primeSizes).foreach((i) => log("N(%,6d) = %30s".format(i, getN(i))))
    log(getSqrt(getN(primeSizes - 1) / 2))
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
  
}
