package eulerProject.solved

/**
 Problem 3.<br>
02 November 2001<br>
<br>
The prime factors of 13195 are 5, 7, 13 and 29.<br>
<br>
What is the largest prime factor of the number 600851475143 ?<br>
 * EULER: SOLVED
 */
object Problem003 {
  
  def getGreatestPrimeFor(n: BigInt): BigInt = {
    
    def reduce(x: BigInt, factor: BigInt): BigInt = if(x % factor != 0) x else reduce(x / factor, factor)
    
    def calculate(value: BigInt, primes: List[BigInt], lastDivisiblePrime: BigInt, x: BigInt): BigInt = {
      null
      /*
      if(PrimesUtils.isPrime(value, primes)) {
        print(value)
        if(n % value == 0) {
          println("+")
          val newX = reduce(x, value)
          if(newX == 1) { 
            value
          } else {
            calculate(value + 2, value :: primes, value, newX)
          }
        } else {
          println("")
          calculate(value + 2, value :: primes, lastDivisiblePrime, x)
        }
      } else {
        calculate(value + 2, primes, lastDivisiblePrime, x)
      }
      */
    }
    
    calculate(2, 2 :: Nil, 2, n)
  }
  
  
  def main(args : Array[String]) : Unit = {
    val n = BigInt(args(0))
    
    val t0 = System.currentTimeMillis
    val result = getGreatestPrimeFor(n)
    val deltaT = System.currentTimeMillis - t0
    
    println("===========================================")
    println("Greatest Prime for " + n + " = " + result)
    println("Time = " + deltaT + " ms")    
  }
}
// 600851475143 
// 100000000000
