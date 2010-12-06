package eulerProject

/**
 * Problem 216: Investigating the primality of numbers of the form 2n^2-1<br>
 * 07 November 2008<br>
 * <br>
 * Consider numbers t(n) of the form t(n) = 2n^2 - 1 with n > 1.
 * The first such numbers are 7, 17, 31, 49, 71, 97, 127 and 161.
 * It turns out that only 49 = 7*7 and 161 = 7*23 are not prime.
 * For n <= 10000 there are 2202 numbers t(n) that are prime.<br>
 * <br>
 * <b>How many numbers t(n) are prime for n <= 50,000,000 ?</b><br>
 * <br>
 */
object Problem216 {
  
  def isT(prime: Long): Boolean = {
    val quadraticN: Int = (prime.intValue + 1) / 2
    val n: Int = Math.sqrt(quadraticN).toInt
    (quadraticN == n * n)
  }
  
  def main(args : Array[String]) : Unit = {
    val n = 10000
    val max = 2 * n * n - 1
    
    val t0 = System.currentTimeMillis
    val result = Utils.getPrimesUntil(max.toLong).size
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
