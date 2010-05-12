package eulerProject.solved

object Problem187a {
 def getSemiPrimesUntil(max: Int): Int = {
    
    def printSemi(n: Long, semi: List[Long]) = println("semi(" + n + ") [" + semi.size + "]")
    
    def getQuantity(primes: List[Long], quantity: Int): Int = primes match {
      case prime :: otherPrimes if(prime * prime > max) => quantity
      case prime :: otherPrimes => 
        val semiPrimesForPrime = primes map (_ * prime) filter (_ < max)
        printSemi(prime, semiPrimesForPrime)
        getQuantity(otherPrimes, quantity + semiPrimesForPrime.size)
      case _ => error("Condição desconhecida: accumulatedQuantity = " + quantity + ", primes = " + primes)
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
