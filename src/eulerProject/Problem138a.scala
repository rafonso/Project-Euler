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
object Problem138a {
  
  def getLegs(quantity: Int): List[BigInt] = {
    
    def getBase(root: BigInt, signal: Int): Option[BigInt] = {
      val (b, r) = ((root + signal * 2) * 2) /% 5
      if(r == 0) Some(b)
      else None
    }
    
    def evaluateLegs(leg: BigInt, previousRoot: Option[BigInt], legs: List[BigInt]): List[BigInt] = {
      if(previousRoot.isDefined) {
        val base = getBase(previousRoot.get, 1)
        if(base.isDefined) {
          println("leg(%,20d, +) = %,20d".format(base.get.longValue, leg.longValue))
          if(legs.size + 1 < quantity) evaluateLegs(leg + 2, None, leg :: legs)
          else leg :: legs
        } else {
          evaluateLegs(leg + 2, None, legs)
        }
      } else {
        val currentRoot = Utils.getSqrt(leg * leg * 5 - 1)
        if(currentRoot.isRight) {
          val base = getBase(currentRoot.right.get, -1)
          if(base.isDefined) {
            println("leg(%,20d, -) = %,20d".format(base.get.longValue, leg.longValue))
            if(legs.size + 1 < quantity) evaluateLegs(leg, Some(currentRoot.right.get), leg :: legs)
            else leg :: legs
          } else {
            evaluateLegs(leg, Some(currentRoot.right.get), legs)
          }
        } else {
          evaluateLegs(leg + 2, None, legs)
        }
      }
    }
    
    evaluateLegs(3, None, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 12
    
    val t0 = System.currentTimeMillis
    val legs = getLegs(max)
    val sum = legs.foldLeft(BigInt(0))(_ + _)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println("Legs: " + legs)
    println(sum)
    println("Total Time: " + deltaT + " ms")
  }
}
