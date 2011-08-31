package eulerProject

object Problem041a {
  
  val MAX_PANDIGITAL = 9876543210L
  
  def isPrime(i: Long, priorPrimes: List[Long]): Boolean = priorPrimes match {
    case Nil => true	// Chegou ao final da lista
    case prime :: otherPrimes if(Math.sqrt(prime) > i) => true	// O primo corrente é maior que a raiz de i.
    case prime :: otherPrimes if(i % prime == 0) => false	// É divisível por um número primo
    case _ :: otherPrimes => isPrime(i, otherPrimes) // Compara com o próximo Primo
  }

  def isPandigital(n: Long): Boolean = {
    
    def repeatDigits(lastDigit: Int, remainder: Long, digitsPresent: Array[Boolean]): Boolean = {
      if(remainder == 0) {
        digitsPresent(lastDigit)
      } else if(digitsPresent(lastDigit)) {
        true
      } else {
        digitsPresent(lastDigit) = true
        repeatDigits((remainder % 10).intValue, remainder / 10, digitsPresent)
      }
    }

    ! repeatDigits((n % 10).intValue, (n / 10), Array(false, false, false, false, false, false, false, false, false, false))
  }
  
    def getPrimesUntil(i: Long, priorPrimes: List[Long], max: Long): List[Long] = {
    if(i >= max) {
      priorPrimes.reverse
    } else if(isPrime(i, priorPrimes)) {
//      println(i)
      getPrimesUntil(i + 2, i :: priorPrimes, max)
    } else {
      getPrimesUntil(i + 2, priorPrimes, max)
    }
  } 
  
  def getGreatestPandigitalPrime(n: Long, primes: List[Long]): Long = {
    if(isPandigital(n)) {
//      print(n + " ")
      if(isPrime(n, primes))
        n
      else
        getGreatestPandigitalPrime(n - 2, primes)
    } else {
      getGreatestPandigitalPrime(n - 2, primes)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val base = 1500000000L
    val limit = Math.sqrt(base).toLong
    
    val t0 = System.currentTimeMillis
    println("Getting Primes until " + limit)
    val primes = getPrimesUntil(3, 2 :: Nil, limit)
    println("There is " + primes.size + " primes for " + base + ". Getting greates pandigital prime")
    val initialValue = if(base % 2 == 0) base - 1 else base 
    val value = getGreatestPandigitalPrime(initialValue, primes)
    val deltaT = System.currentTimeMillis - t0
    
    println("\nValue for " + base + " (" + limit + "): " + value)
    println("Total Time: " + deltaT + " ms")
  }
}
