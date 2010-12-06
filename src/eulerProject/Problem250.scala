package eulerProject

/**
 * Problem 250: 250250<br/>
 * 13 June 2009<br/>
 * <br/>
 * <b>Find the number of non-empty subsets of {1<sup>1</sup>, 2<sup>2</sup>, 
 * 3<sup>3</sup>,..., 250250<sup>250250</sup>}, the sum of whose elements is 
 * divisible by 250. Enter the rightmost 16 digits as your answer.</b><br/>
 * 
 */
object Problem250 {
  
  val divisor = 250
  val POW10 = BigInt("1" + "0" * 16)
  val ZERO = BigInt(0)
  val ONE = BigInt(1)
  
  def getPow(base: Int): BigInt = {
    
    def eval(index: Int, pow: BigInt): BigInt = {
      if(pow % POW10 == 0) {
        ZERO
      } else if(index == base) {
        println("\t" + base + "^" + base + " = " + pow)
        pow
      } else {
        eval(index + 1, pow * base)
      }
    }
    println(base)
    eval(0, ONE)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 250250
    
    val t0 = System.currentTimeMillis
    val result = (10 to max by 10).foldLeft(ZERO)(_ + getPow(_))
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println("SUM    = " + result)
    println("RESULT = " + result % POW10)
    println("Total Time: " + deltaT + " ms")

  }
}