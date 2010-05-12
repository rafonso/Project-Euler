package eulerProject.solved

/**
 * Problem 63: How many n-digit positive integers exist which are also an nth power?<br>
 * 13 February 2004<br>
 * <br>
 * The 5-digit number, 16807=7^(5), is also a fifth power. Similarly, the 9-digit number, 134217728=8^(9), is a ninth power.<br>
 * <br>
 * <b>How many n-digit positive integers exist which are also an nth power?</b><br>
 * 	
 */
object Problem063 {

  type numPow = BigInt
  type triplet = (Int, Int, numPow)
  val ONE = BigInt(1)
    //BigInt(1)
  
  def evaluateN(n: Int): List[triplet] = {
    
    def eval(base: Int, results: List[triplet]): List[triplet] = {
      val power = (1 to n).foldLeft(ONE)((pow, i) => pow * base)
      val digitsNumber = power.toString.size
      println("%d^%d = %s".format(base, n, power))
      if(digitsNumber > n) results.reverse
      else if(digitsNumber < n) eval(base + 1, Nil)
      else eval(base + 1, (base, n, power) :: results)
    }
    
    eval(1, Nil)
  }  
  
  def main(args : Array[String]) : Unit = {
    val max = 100
    val t0 = System.currentTimeMillis
    val result = (1 to max).flatMap(evaluateN(_)).force
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println(result.size)
    println("Total Time: " + deltaT + " ms")
    
  }
}
