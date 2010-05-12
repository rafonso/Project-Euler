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
 * Find Sum(L) for the twelve smallest isosceles triangles for which 
 * h = b +- 1 and b, L are positive integers.<br/>
 * <br/>
 */
object Problem138 {
  
  def getLegs(max: Int): List[Long] = {
    
    def getLeg(b: BigInt, add: Boolean): Option[BigInt] = {
      val signal = if(add) +1 else -1
      val numerator = b * b * 5 + b * signal * 8 + 4
      val (squareL, remainder) = numerator /% 4
      if(remainder == 0) Utils.getSqrt(squareL).right.toOption 
      else None
    }
    
    def evaluateBase(base: Long, add: Boolean, legs: List[Long]): List[Long] = {
      val optLeg = getLeg(base, add)
      if(optLeg.isDefined) {
        println("leg(%,20d, %5s) = %,20d".format(base, add, optLeg.get.longValue))
        val nextLegs = optLeg.get.longValue :: legs
        if(legs.size + 1 == max) nextLegs
        else if(add) evaluateBase(base + 1, false, nextLegs)
        else evaluateBase(base, true, nextLegs)
      } else if(add) evaluateBase(base + 1, false, legs)
        else evaluateBase(base, true, legs)
    }
    
    evaluateBase(1, false, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 5
    
    val t0 = System.currentTimeMillis
    val legs = getLegs(max)
    val sum = legs.foldLeft(0L)(_ + _)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println("Legs: " + legs)
    println(sum)
    println("Total Time: " + deltaT + " ms")
  }
}
/*
leg(                  16, false) =                   17
leg(                 272,  true) =                  305
leg(               4.896, false) =                5.473
leg(              87.840,  true) =               98.209
leg(           1.576.240, false) =            1.762.289
*/