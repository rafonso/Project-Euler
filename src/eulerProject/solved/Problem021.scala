package eulerProject.solved

import scala.collection.immutable._

/**
 * Problem 21: Evaluate the sum of all amicable pairs under 10000.<br>
 * 05 July 2002<br>
 * <br>
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n
 * which divide evenly into n).<br>
 * If d(a) = b and d(b) = a, where a != b, then a and b are an amicable pair and
 * each of a and b are called amicable numbers.<br>
 * <br>
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
 * 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
 * 71 and 142; so d(284) = 220.<br>
 * <br>
 * <b>Evaluate the sum of all the amicable numbers under 10000.</b>
 * 
 */
object Problem021 {
  
  def getDivisors(n: Int): Set[Int] = {
    
    def generateDivisors(x: Int, divisors: Set[Int]): Set[Int] = {
      if(x > n / 2) divisors
      else if(divisors.contains(x)) generateDivisors(x + 1, divisors)
      else {
        val (division, remainder) = eulerProject.Utils./%(n, x)
        if(remainder == 0) generateDivisors(x + 1, divisors + (x, division))
        else generateDivisors(x + 1, divisors)
      }
    }
    
    generateDivisors(2, TreeSet.empty) + 1
  }
  
  def getSum(n: Int) = getDivisors(n).foldLeft(0)(_ + _)
  
  def generateTable(i: Int, max: Int, values: Map[Int, Int]): Map[Int, Int] = 
    if(i > max) values 
    else generateTable(i + 1, max, values + ((i, getSum(i))))
  
  def getAmigables(values: Map[Int, Int]): List[(Int, Int)] = {
    
    def eval(vals: List[(Int, Int)], amicables: List[(Int, Int)]): List[(Int, Int)] = vals match {
      case Nil => amicables.reverse
      case (a, b) :: others if(a == b) => eval(others, amicables)
      case (a, b) :: others  => {
        val optD = values.get(b)
//        println("d(" + a + ") = " + b + "\td(" + b + ") = " + optD)
        if(optD.isDefined && (optD.get == a)) eval(others - (b, a), (a, b) :: amicables)
        else eval(others, amicables)
      }
    }
    
    eval(values.toList, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
      //10000
    
    val t0 = System.currentTimeMillis
    val values = generateTable(1, max, IntMap.empty)
    println(values.size)
    val amicables = getAmigables(values)
    println(amicables)
    val summer = (sum: Int, pair: (Int, Int)) => sum + pair._1 + pair._2
    val result = amicables.foldLeft(0)(summer(_, _))
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================================================")
    println(result)
    println("Time = " + deltaT + " ms")

  }
}
