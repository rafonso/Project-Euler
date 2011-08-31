package eulerProject

import scala.collection.immutable._

/**
 * Problem 47: Find the first four consecutive integers to have four distinct 
 * primes factors.<br/>
 * 04 July 2003<br/>
 * <br/>
 * The first two consecutive numbers to have two distinct prime factors are:<br/>
 * <br/>
 * 14 = 2 × 7<br/>
 * 15 = 3 × 5<br/>
 * <br/>
 * The first three consecutive numbers to have three distinct prime factors are:<br/>
 * <br/>
 * 644 = 2² × 7 × 23<br/>
 * 645 = 3 × 5 × 43<br/>
 * 646 = 2 × 17 × 19.<br/>
 * <br/>
 * <b>Find the first four consecutive integers to have four distinct primes 
 * factors. What is the first of these numbers?</b><br/>
 * <br/>
 */
object Problem047 {
  
  def getPrimesDivisorsFor(n: Long, primes: List[Long], limit: Int, maxQuantity: Int, divisors: Set[Long]): Option[Set[Long]] = {
    println(n + ", " + primes.first + ", " + divisors)
    primes match {
      case prime :: others if((prime > limit) && (divisors.size == maxQuantity)) => Some(divisors + n)
      case prime :: others if((prime > limit) && (divisors.size != maxQuantity)) => None
      case prime :: others => {
        val (quocient, remainder) = Utils./%(n, prime)
        remainder match {
          case 0 if(divisors.size + 1 >  maxQuantity) => None
          case 0 if(divisors.size + 1 <= maxQuantity) => {
            val nextDivisors = divisors + prime
            primes.contains(quocient) match {
              case false => getPrimesDivisorsFor(quocient, prime :: others, limit, maxQuantity, nextDivisors)
              case true if(nextDivisors.size + 1 == maxQuantity) => Some(nextDivisors + quocient)
              case true if(nextDivisors.size + 1 != maxQuantity) => None
            }
          }
          case _ => getPrimesDivisorsFor(n, others, limit, maxQuantity, divisors)
        }
      }
      case Nil => error("I need more Primes!")
    }
  }
  
  
  
  def main(args : Array[String]) : Unit = {
    val primes = Utils.getPrimesUntil(1000L)
    val n = 646
    val limit = Math.sqrt(n).toInt
    val maxQuantity = 3
    
    println(getPrimesDivisorsFor(n, primes, limit, maxQuantity, TreeSet.empty[Long]))
  }
}
