package eulerProject

/**
 <b>Problem 41</b><br>
11 April 2003<br>
<br>
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.<br>
<br>
What is the largest n-digit pandigital prime that exists?<br>

 */
object Problem041 {
  
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
  
  def getGreatestPandigitalPrimeUntil(limit: Long): Long = {    
    
    def getValue(number: Long, currentPandigital: Long, primes: List[Long]): Long = {
      if(number > limit) {
        currentPandigital
      } else if(isPrime(number, primes)) {
        if(isPandigital(number)) {
          println(number)
          getValue(number + 2, number, number :: primes)
        } else {
          getValue(number + 2, currentPandigital, number :: primes)
        }
      } else {
        if(number % 999999 == 0)
          print(" " + number)
        getValue(number + 2, currentPandigital, primes)
      }
    }
    
    
    getValue(3, 0, 2 :: Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val n = args(0).toLong
    
    val t0 = System.currentTimeMillis
    val pandigital = getGreatestPandigitalPrimeUntil(n)
    println("")
    println("f(" + n + ") = " + pandigital)
    val deltaT = System.currentTimeMillis - t0
    
    println("Time = " + deltaT + " ms")
  }
}
