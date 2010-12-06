package eulerProject

/**
 * Problem 152: Writing 1/2 as a sum of inverse squares.<br/>
 * 27 April 2007<br/>
 * <br/>
 * There are several ways to write the number 1/2 as a sum of inverse squares 
 * using distinct integers.<br/>
 * <br/>
 * For instance, the numbers {2,3,4,5,7,12,15,20,28,35} can be used:<br/>
 * <br/>
 * <img src="http://projecteuler.net/project/images/p_152_sum.gif" alt="" border="0"/><br/>
 * <br/>
 * In fact, only using integers between 2 and 45 inclusive, there are exactly 
 * three ways to do it, the remaining two being: {2,3,4,6,7,9,10,20,28,35,36,45} 
 * and {2,3,4,6,7,9,12,15,28,30,35,36,45}.<br/>
 * <br/>
 * <b>How many ways are there to write the number 1/2 as a sum of inverse 
 * squares using distinct integers between 2 and 80 inclusive?</b><br/>
 * <br/>
 */
object Problem152a {
  
  import Utils._
  
  def main(args : Array[String]) : Unit = {
    val sequence = (2 to 45).map(i => i * i).reverse
    val maxCombinations = Math.pow(2, sequence.size).toLong
     println(sequence)
     println(maxCombinations)
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = max
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
