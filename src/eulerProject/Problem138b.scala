package eulerProject

/**
 * Problem 138: Investigating isosceles triangle for which the height and base 
 * length differ by one.<br/>
 * 20 January 2007<br/>
 * <br/>
 * Consider the isosceles triangle with base length, b = 16, and legs, 
 * L = 17.<br/>
 * <div style="text-align: center;">
 * <img src="http://projecteuler.net/images/p_138.gif" alt="" height="228" width="230">
 * </div>
 * <br/>
 * By using the Pythagorean theorem it can be seen that the height of the 
 * triangle, h = Sqrt(17<sup>2</sup> - 8<sup>2</sup>) = 15, which is one less 
 * than the base length.<br/>
 * <br/>
 * With b = 272 and L = 305, we get h = 273, which is one more than the base 
 * length, and this is the second smallest isosceles triangle with the 
 * property that h = b +- 1.<br/>
 * <br/>
 * <b>Find Sum(L) for the twelve smallest isosceles triangles for which 
 * h = b +- 1 and b, L are positive integers.</b><br/>
 * <br/>
 */
object Problem138b {
  
  import Utils._
  
  def getLegs(maxQty: Int): List[BigInt] = {
    
    def getNewValues(b: BigInt, delta: Int): (BigInt, Int) = delta match {
      case  1 => (b + 2, -1)
      case -1 => (b    ,  1)
      case  _ => error("delta irregular: " + delta + ". b = " + b)
    }
    
    def evaluateB(b: BigInt, delta: Int, legs: List[BigInt]): List[BigInt] = {
      getSqrt(b * b * 5 / 4 + b * 2 * delta + 1) match {
        case Right(l) => {
          log("b = %,16d, delta = %+d, l = %,16d".format(b.longValue, delta, l.longValue))
          if(legs.size == maxQty - 1) l :: legs
          else {
            val (nextB, nextDelta) = getNewValues(b, delta)
            evaluateB(nextB, nextDelta, l :: legs)
          }
        }
        case Left(_) => {
          val (nextB, nextDelta) = getNewValues(b, delta)
          evaluateB(nextB, nextDelta, legs)
        }
      }
    }
    
    evaluateB(2, -1, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 12
    
    val t0 = System.currentTimeMillis
    val result = getLegs(max)
    val sum = result.foldLeft(BigInt(0))(_ + _)
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log(sum)
    log("Total Time: " + deltaT + " ms")
  }
}