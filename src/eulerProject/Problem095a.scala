package eulerProject

import scala.collection.immutable._

object Problem095a {
  
  type Numbers = List[Int]
  
  def getDivisors(n: Int): Numbers = {
    
    def generateDivisor(i: Int, divisors: Set[Int]): Numbers = {
      if(i < n / 2) {
        if(divisors(i)) {
          generateDivisor(i + 1, divisors)
        } else {
          val (quocient, remainder) = Utils./%(n, i)
          val nextDivisors = if(remainder == 0) (divisors + (i, quocient)) else divisors
          generateDivisor(i + 1, nextDivisors)
        }
      } else {
        divisors.toList
      }
    }
    
    generateDivisor(2, TreeSet(1))
  }
  
  def sumDivisors(n: Int) = getDivisors(n).foldLeft(0)(_ + _)
  
  def getDivisorsSum(max: Int): Map[Int, Int] = {
    
    def evaluateN(n: Int, sumByN: Map[Int, Int]): Map[Int, Int] = {
      if(n <= max) {
        val sum = sumDivisors(n)
        if(sum <= max) evaluateN(n + 1, sumByN + ((n, sum)))
        else evaluateN(n + 1, sumByN)
      } else {
        sumByN
      }
    }
    
    evaluateN(1, TreeMap.empty[Int, Int])
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 100000
    
    val t0 = System.currentTimeMillis
    val map = getDivisorsSum(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(map.size)
    println("Time = " + deltaT + " ms")
  }
}
