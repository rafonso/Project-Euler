package eulerProject.solved

/**
 * Problem 120: Finding the maximum remainder when (a - 1)<sup>n</sup> 
 * + (a + 1)<sup>n</sup> is divided by a<sup>2</sup>.<br>
 * 21 April 2006<br>
 * <br>
 * Let r be the remainder when (a - 1)<sup>n</sup> + (a + 1)<sup>n</sup> is 
 * divided by a<sup>2</sup>.<br>
 * <br>
 * For example, if a = 7 and n = 3, then r = 42: 6<sup>3</sup> + 8<sup>3</sup> 
 * = 728 = 42 mod 49. And as n varies, so too will r, but for a = 7 it turns out 
 * that r<sub>max</sub> = 42.<br>
 * <br>
 * For 3 <= a <= 1000, find sum(r<sub>max</sub>).<br>
 * <br>
 * EULER: SOLVED
 */
object Problem120 {
  
  val ONE = BigInt(1)
  
  def pow(x: Int, n: Int) = (1 to n).foldLeft(ONE)((product, i) => product * x)
  
  def getRemainder(a: Int, n: Int): BigInt = (pow(a - 1, n) + pow(a + 1, n)) % pow(a, 2)
  
  def getMax(max: BigInt, x: BigInt): BigInt = if(x > max) x else max
  
  def maxRemainder(a: Int): Int = {
    val max = a match {
      case x if x % 2 != 0 => a * 2
      case x if x % 4 == 0 => a / 2
      case _ => a
    }
    
    val remainders = (1 to max).map(getRemainder(a, _))
    println(a + " => " + remainders)
    remainders.foldLeft(ONE)(getMax(_, _)).intValue
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val maxRemainders = (3 to 20).map(maxRemainder(_)).force
    val sum = maxRemainders.foldLeft(0)(_+_)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(maxRemainders)
    println(sum)
    println("Time = " + deltaT + " ms")
  }
}
