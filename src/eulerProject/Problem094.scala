package eulerProject

import java.util.concurrent._
import scala.actors.Futures._

/**
 * Problem 94: Investigating almost equilateral triangles with integral sides 
 * and area.<br>
 * 29 April 2005<br>
 * <br>
 * It is easily proved that no equilateral triangle exists with integral 
 * length sides and integral area. However, the almost equilateral 
 * triangle 5-5-6 has an area of 12 square units.<br>
 * <br>
 * We shall define an almost equilateral triangle to be a triangle for which 
 * two sides are equal and the third differs by no more than one unit.<br>
 * <br>
 * <b>Find the sum of the perimeters of all almost equilateral triangles with 
 * integral side lengths and area and whose perimeters do not exceed one 
 * billion (1,000,000,000).</b><br>
 * <br>
 */
object Problem094 {
  
  case class Triangle(side: BigInt, add: Boolean) {
    
    private val delta = if(add) 1 else - 1
    
    import Utils._
    
    private def getArea: Option[BigInt] = {
      val numerator = (side - delta) * (side + delta) * (side + delta) * (side * 3 + delta)
      val (div, rem) = numerator /% 16L
      if(rem != 0) None
      else {
        val sqrt = getSqrt(div)
        if(sqrt.isRight) Some(sqrt.right.get)
        else None
      }
    }

    val perimeter = side * 3 + delta
    
    lazy val area: Option[BigInt] = this.getArea 
    
  }
  
  def showReturn(t: Triangle): Triangle = {
    printf("%25s => %16s -> %13s %n", t, t.area.get, t.perimeter)
    t
  }
  
  def printSize(size: Int)= {
    if((size + 1) % 10000 == 0) {
      val now = new java.util.Date
      println("[%tT] %,10d".format(now, size))      
    }
    size
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 100000
    
    val t0 = System.currentTimeMillis
    val result = (1 to max by 2)
      .map(printSize(_))
      .flatMap(l => List(Triangle(l, false), Triangle(l, true)))
      .filter(_.area.isDefined)
      .map(showReturn(_))
      .foldLeft(BigInt(0))(_ + _.perimeter)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }

/*
163767371468
Time = 257681 ms
 */
}