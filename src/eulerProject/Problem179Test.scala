package eulerProject

object Problem179Test {
  
  val primeIterator = new PrimeIterator
  primeIterator.iterateUntil(1000)
  
  def getDivisors(n: Long): List[(Long, Int)] = {
    
    def divideNumberByPrime(x: Long, prime: Long, times: Int): (Long, Int) = 
      if(x % prime == 0) divideNumberByPrime(x / prime, prime, times + 1)
      else (x, times)
    
    def iteratePrimes(x: Long, disponiblePrimes: List[Long], divisorsQuantity: List[(Long, Int)]): List[(Long, Int)] = {
      if(x == 1) divisorsQuantity
      else disponiblePrimes match {
        case Nil => error("There are more primes than n")
        case prime :: otherPrimes if x % prime != 0 => iteratePrimes(x, otherPrimes, divisorsQuantity)
        case prime :: otherPrimes if x == prime => (prime, 1) :: divisorsQuantity
        case prime :: otherPrimes if x % prime == 0 => {
          val nextXTimes = divideNumberByPrime(x, prime, 0)
          iteratePrimes(nextXTimes._1, otherPrimes, (prime, nextXTimes._2) :: divisorsQuantity)
        }
        case _ => error("disponiblePrimes irregular: " + disponiblePrimes)
      }
    }
    
    iteratePrimes(n, primeIterator.getAccumulatedPrimes, Nil).reverse
  }
  
  
  def getQuantityOfDivisors(n: Int): Int = {
    def sum(acc: Int, primeQuantity: (Long, Int)) = acc * (primeQuantity._2 + 1)
    
    getDivisors(n).foldLeft(1)(sum(_, _))
  }
  
  def main(args : Array[String]) : Unit = {
    val n = 3590
    println(getDivisors(n))
    println(getQuantityOfDivisors(n))
  }
}
