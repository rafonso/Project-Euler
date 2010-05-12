package eulerProject

import scala.collection.immutable.Set
import scala.actors.Futures._
import java.util.concurrent._

/**
 * Problem 131: Determining primes, p, for which n^3 + n^2*p is a perfect cube.<br/>
 * 10 November 2006<br/>
 * <br/>
 * There are some prime values, p, for which there exists a positive integer, 
 * n, such that the expression n^(3) + n^(2)p is a perfect cube.<br/>
 * <br/>
 * For example, when p = 19, 8^(3) + 8^(2)×19 = 12^(3).<br/>
 * <br/>
 * What is perhaps most surprising is that for each prime with this property 
 * the value of n is unique, and there are only four such primes below 
 * one-hundred.<br/>
 * <br/>
 * <b>How many primes below one million have this remarkable property?</b><br/>
 * <br/>
 */
object Problem131a {
  
  
  type NPrimeBaseCube = (Long, Long, Long, Long)
  
  def printResult(result: NPrimeBaseCube) = println("base = %,9d, n = %,9d, prime = %,9d, cube = %,25d".format(result._3, result._1, result._2, result._4))
  
  def getResultForBase(base: Long, cube: Long, primes: Set[Long], n: Long): Option[NPrimeBaseCube] = {
    if(n < base) {
      val (quocient, remainder) = Utils./%(cube - n * n * n, n * n)
      if((remainder == 0) && primes(quocient)) {
        val result = (n, quocient, base, cube)
        printResult(result)
        Some(result)
      }
      else getResultForBase(base, cube, primes, n + 1)
    } else {
      None
    }
  }
  
  def createFuture(base: Int, primes: Set[Long]): Callable[Boolean] = new Callable[Boolean]{ def call = getResultForBase(base, base * base * base, primes, 1).isDefined }
  
  def getQuantityOfPrimes(max: Int): Int = {
    val listPrimes: List[Long] = Utils.getPrimesUntil(max)
    val primes = Set.empty[Long] ++ listPrimes
    val maxBase = listPrimes.last + 1
    
    val pool = Executors.newFixedThreadPool(10)
    
    (1 to max)
//      .map(base => getResultForBase(base, base * base * base, primes, 1)).filter(_.isDefined)
      .map(base => pool.submit(createFuture(base, primes)).get).filter(_ == true)
      .size
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 10000
    
    val t0 = System.currentTimeMillis
    val result = getQuantityOfPrimes(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println("Result: " + result)
    println("Time = " + deltaT + " ms")
  }
}