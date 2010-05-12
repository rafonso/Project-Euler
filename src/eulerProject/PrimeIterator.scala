package eulerProject

class PrimeIterator extends BufferedIterator[Long] {
  private val primes = new scala.collection.mutable.ListBuffer[Long]
  primes += 2
  private var lastPrime = 2L
  private var delta = 1
  
  private def isPrime(index: Int, n: Long): Boolean = {
    val currentPrime = primes(index)
    if(currentPrime * currentPrime > n) true
    else if(n % currentPrime == 0) false
    else isPrime(index + 1, n)
  }
  
  private def getNextPrime(n: Long): Long = 
    if(isPrime(0, n)) n
    else getNextPrime(n + 2)
  
  def head = this.lastPrime
  
  def hasNext = true
  
  def next = {
    this.lastPrime = this.getNextPrime(this.head + delta)
    primes += this.lastPrime
    delta = 2
    this.lastPrime
  }
  
  def getAccumulatedPrimes = primes.toList
  
  @throws(classOf[IllegalArgumentException])
  def isPrime(n: Long): Boolean = n match {
    case 1 => false
    case 2 => true
    case x if(x % 2 == 0) => false
    case x if(x > this.head * this.head) => throw new IllegalArgumentException()
    case x if(this.primes.contains(x)) => true
    case _ => this.isPrime(0, n)
  }
  
  def iterateUntil(limit: Long): PrimeIterator = {
    while(this.head < limit) this.next
    if(this.head > limit) {
      this.primes - this.head
      this.lastPrime = this.primes.last
    }
    this
  }
}