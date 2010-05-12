package eulerProject

import scala.collection.immutable._

/**
 * Problem 95: Find the smallest member of the longest amicable chain with no 
 * element exceeding one million.<br/>
 * 13 May 2005<br/>
 * <br/>
 * The proper divisors of a number are all the divisors excluding the number 
 * itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As 
 * the sum of these divisors is equal to 28, we call it a perfect number.<br/>
 * <br/>
 * Interestingly the sum of the proper divisors of 220 is 284 and the sum of 
 * the proper divisors of 284 is 220, forming a chain of two numbers. For this 
 * reason, 220 and 284 are called an amicable pair.<br/>
 * <br/>
 * Perhaps less well known are longer chains. For example, starting with 
 * 12496, we form a chain of five numbers:<br/>
 * <br/>
 * 12496 -> 14288 -> 15472 -> 14536 -> 14264 (-> 12496 -> ...)<br/>
 * <br/>
 * Since this chain returns to its starting point, it is called an amicable 
 * chain.<br/>
 * <br/>
 * <b>Find the smallest member of the longest amicable chain with no element 
 * exceeding one million.</b><br/>
 * <br/>
 */
object Problem095 {
  
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
  
  def getChain(x: Int, maxValue: Int, invalids: Set[Int], chain: Numbers): Either[Numbers, Numbers] = {
    if((chain.size > 0) && (x == chain.last)) Right(chain)
    else if((x > maxValue) || invalids(x)) Left(x :: chain)
    else if(x == 1) Left(chain)
    else if(chain.contains(x)) {
      chain.dropWhile(_ != x) match {
        case x :: others => Left(others)
        case _ => error("Erro ao extrair o ciclo de uma corrente. Elemento repetido: " + x + ". Corrente: " + chain.toString)
      }
    }
    else getChain(sumDivisors(x), maxValue, invalids, x :: chain)
  }
  
  def getGreatestChain(elementMaxValue: Int): Numbers = {
    
    def evaluateN(n: Int, validNumbers: Set[Int], invalidNumbers: Set[Int], greatestChain: Numbers): Numbers = {
      if(n <= elementMaxValue) {
        if(n % 2000 == 0) println("-%,7d".format(n))
        if(validNumbers(n) || invalidNumbers(n)) {
          evaluateN(n + 1, validNumbers, invalidNumbers, greatestChain)
        } else getChain(n, elementMaxValue, invalidNumbers, Nil) match {
          case Right(valids) => {
            println("!%,7d: %s".format(n, valids.reverse))
            val nextChain = if(valids.size > greatestChain.size) valids else greatestChain
            evaluateN(n + 1, validNumbers ++ valids, invalidNumbers, nextChain)
          }
          case Left(invalids) => {
//            println("!%,7d => NO => %s".format(n, invalids.reverse))
            evaluateN(n + 1, validNumbers, invalidNumbers ++ invalids, greatestChain)
          }
        }
      } else {
        greatestChain
      }
    }
    
    evaluateN(1, HashSet.empty[Int], HashSet.empty[Int], Nil)
  }
  
  def getSmallest(numbers: Numbers, smallest: Int): Int = numbers match {
    case first :: others if(first <  smallest) => getSmallest(others, first)
    case first :: others if(first >= smallest) => getSmallest(others, smallest)
    case Nil => smallest
    case _ => error(numbers.toString)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000000
    
    val t0 = System.currentTimeMillis
    val chain = getGreatestChain(max)
    val smallest = getSmallest(chain, Math.MAX_INT)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println("Greatest Chain: " + chain.reverse)
    println("Smallest: " + smallest)
    println("Time = " + deltaT + " ms")
  }
}
