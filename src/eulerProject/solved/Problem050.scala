package eulerProject.solved

/**
 * Problem 50: Which prime, below one-million, can be written as the sum of the 
 * most consecutive primes?<br>
 * 15 August 2003<br>
 * <br>
 * The prime 41, can be written as the sum of six consecutive primes:<br>
 * 41 = 2 + 3 + 5 + 7 + 11 + 13<br>
 * <br>
 * This is the longest sum of consecutive primes that adds to a prime below 
 * one-hundred.<br>
 * <br>
 * The longest sum of consecutive primes below one-thousand that adds to a 
 * prime, contains 21 terms, and is equal to 953.<br>
 * <br>
 * <b>Which prime, below one-million, can be written as the sum of the most 
 * consecutive primes?</b><br>
 * <br>
 * EULER: SOLVED
 */
object Problem050 {
  
  def getSum(primes: List[Long], from: Int, to: Int): Long = 
    primes.slice(from, to).foldLeft(0L)(_ + _)
  
  def getSum(primes: List[Long]): Long = getSum(primes, 0, primes.size)
  
  def isPrime(n: Long, primes: List[Long]) = primes.contains(n)
  
  
  def getRangeOfPrimes(primes: List[Long], from: Int, max: Int): List[Long] = {
    
    def evaluate(to: Int, greatestSum: Long, lastPosition: Int): Int = {
      if(to == primes.size) {
        lastPosition
      } else {
        val sum = getSum(primes, from, to)
        if(sum > max) {
          lastPosition
        } else if(isPrime(sum, primes) && sum > greatestSum) {
          evaluate(to + 1, sum, to)
        } else {
          evaluate(to + 1, greatestSum, lastPosition)
        }
      }
    }
    
    primes.slice(from, evaluate(from, 0, from))
  }
  
  def function2(primes: List[Long], max: Int): List[Long] = {
    
    def evaluate2(from: Int, accumulatedPrimes: List[Long]): List[Long] = {
      if(from == primes.size) {
        accumulatedPrimes
      } else {
        val currentPrimes = getRangeOfPrimes(primes, from, max)
        
        if(currentPrimes.size < accumulatedPrimes.size) {
          evaluate2(from + 1, accumulatedPrimes)
        } else if(currentPrimes.size == accumulatedPrimes.size) {
          if(getSum(currentPrimes) > getSum(accumulatedPrimes)) {
            println(from + ": " + getSum(currentPrimes) + "-> " + currentPrimes)
            evaluate2(from + 1, currentPrimes)
          } else {
            evaluate2(from + 1, accumulatedPrimes)
          }
        } else {
          println(from + ": " + getSum(currentPrimes) + "-> " + currentPrimes)
          evaluate2(from + 1, currentPrimes)
        }
      }
    }
    
    evaluate2(0, List(2))
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000000
    val primes = Utils.getPrimesUntil(max)
    
    val t0 = System.currentTimeMillis
    val result = function2(primes, max)
    val deltaT = System.currentTimeMillis - t0
    
    println("Primes: " + result)
    println("Size: " + result.size)
    println("Sum = " + getSum(result))
    println("Time = " + deltaT + " ms")
  }
}
