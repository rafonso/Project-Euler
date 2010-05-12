package eulerProject.solved

/**
 * Problem 187: Semiprimes<br>
 * 22 March 2008<br>
 * <br>
 * A composite is a number containing at least two prime factors. For example, 
 * 15 = 3 × 5; 9 = 3 × 3; 12 = 2 × 2 × 3.<br>
 * <br>
 * There are ten composites below thirty containing precisely two, not 
 * necessarily distinct, prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.<br>
 * <br>
 * <b>How many composite integers, n < 10<sup>8</sup>, have precisely two, not 
 * necessarily distinct, prime factors?</b><br>
 * <br>
 */
object Problem187 {
  
  
  def getSemiPrimesUntil(max: Int): Int = {
    
    def getQuantityForPrime(prime: Long, myPrimes: List[Long], qtyForPrime: Int): Int = myPrimes match {
      case currentPrime :: otherPrimes if (prime * currentPrime > max) => qtyForPrime
      case currentPrime :: otherPrimes => 
//        println("\t" + prime + " * " + currentPrime + " = " + (prime * currentPrime))
        getQuantityForPrime(prime, otherPrimes, qtyForPrime + 1)
      case Nil => qtyForPrime // error("Chegou ao final da relação de primos antes de chegar ao final!! prime = " + prime + ", qtyForPrime = " + qtyForPrime)
      case _ => error("Condição desconhecida: prime = " + prime + ", qtyForPrime = " + qtyForPrime + ", myPrimes = " + myPrimes)
    }
    
    def getQuantity(primes: List[Long], accumulatedQuantity: Int): Int = primes match {
      case prime :: otherPrimes if(prime * prime > max) => accumulatedQuantity
      case prime :: otherPrimes => 
        val quantityForPrime = getQuantityForPrime(prime, primes, 0)
        println("qty(" + prime + ") = " + quantityForPrime)
        getQuantity(otherPrimes, quantityForPrime + accumulatedQuantity)
      case _ => error("Condição desconhecida: accumulatedQuantity = " + accumulatedQuantity + ", primes = " + primes)
    }
    
    
    println("Getting primes until " + (max / 2))
    val primes = Utils.getPrimesUntil(max / 2)
    println("I have " + primes.size + " primes")
    
    getQuantity(primes, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 100000000
    
    val t0 = System.currentTimeMillis
    val result = getSemiPrimesUntil(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("=================================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
