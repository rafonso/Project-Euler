package eulerProject

/**
 * Problem 251: Cardano Triplets.<br>i
 * 20 June 2009<br>
 * <br>
 * A triplet of positive integers (a,b,c) is called a Cardano Triplet if it 
 * satisfies the condition:<br>
 * cubeSquare(a + b * root(c)) + cubeSquare(a - b * root(c)) = 1<br>
 * For example, (2,1,5) is a Cardano Triplet.<br>
 * <br>
 * There exist 149 Cardano Triplets for which a+b+c <= 1000.<br>
 * <br>
 * <b>Find how many Cardano Triplets exist such that a+b+c <= 110,000,000.</b><br>
 * <br>
 * Note: This problem has been changed recently, please check that you are 
 * using the right parameters.<br>
 * <br>
 * <hr>
 * <pre>
 * Solution:
 * cbrt(a + b * sqrt(c)) + cbrt(a - b * sqrt(c)) = 1 <--> 
 * c = (8 * a ^ 3 + 15 * a ^ 2 + 6 * a - 1)/ (27 * b ^ 2) (Solved in Wolfram Alpha)
 * Or:
 * c = n / (27 * b ^ 2) (1)
 * Where: 
 * n = 8 * a ^ 3 + 15 * a ^ 2 + 6 * a - 1 (2)
 * For c > 1, from (1):
 * b < sqrt(n / 27) (3)
 * For:
 * a + b + c <= L
 * 
 * </pre>
 */
object Problem251a {
  import Utils._
  
  type Cardano = (Int, Int, Int)
  type OptCardano = Option[Cardano]
  
  def evaluateA(a: Int, max: Int): Int = {
    
    val num = 8 * a * a * a + 15 * a * a + 6 * a - 1
    
    def evaluateB(b: Int, qty: Int): Int = {
      if(b < 1) {
        qty
      } else {
        val (c, remC) = /%(num, 27 * b * b)
        if(remC == 0 && (a + b + c < max)) {
          log("%,11d + %,11d + %,11d = %,11d".format(a, b, c, (a + b + c)))
          evaluateB(b - 1, qty + 1)
        } else {
          evaluateB(b - 1, qty)
        }
      }
    }
    
    val bMax = Math.sqrt(num / 27).toInt
    evaluateB(bMax, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 110000000
    
    val t0 = System.currentTimeMillis
    val result = (1 to max).foldLeft(0)((sum, a) => sum + evaluateA(a, max))
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
