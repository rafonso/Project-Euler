package eulerProject.solved

/**
 * Problem 46: What is the smallest odd composite that cannot be written as 
 * the sum of a prime and twice a square?
 * 20 June 2003<br/>
 * <br/>
 * It was proposed by Christian Goldbach that every odd composite number can 
 * be written as the sum of a prime and twice a square.<br/>
 * <br/>
 * 9  =  7 + 2×1^2<br/>
 * 15 =  7 + 2×2^2<br/>
 * 21 =  3 + 2×3^2<br/>
 * 25 =  7 + 2×3^2<br/>
 * 27 = 19 + 2×2^2<br/>
 * 33 = 31 + 2×1^2<br/>
 * <br/>
 * It turns out that the conjecture was false.<br/>
 * <br/>
 * <b>What is the smallest odd composite that cannot be written as the sum 
 * of a prime and twice a square?</b><br/>
 * 
 */
object Problem046 {
  
  type CompositeOrNot = Either[Long, (Long, Long)]
  
  val primes = eulerProject.Utils.getPrimesUntil(10000L)
  
  def getSqrt(n: Long): Either[Double, Long] = {
    val sqrtDbl = Math.sqrt(n)
    val sqrtLng = sqrtDbl.toLong
    if(sqrtLng * sqrtLng == n) Right(sqrtLng)
    else Left(sqrtDbl)
  }
  
  def isComposite(n: Long): CompositeOrNot = {
    
    def evalPrime(currentPrimes: List[Long]): CompositeOrNot = currentPrimes match {
      case Nil => error("I need more primes to evaluate")
      case p :: others if(p >= n) => Left(n)
      case p :: others => {
        val sqrt = getSqrt((n - p) / 2)
        if(sqrt.isRight) Right(p, sqrt.right.get)
        else evalPrime(others)
      }
    }
    
    evalPrime(primes)
  }
  
  def getNotComposite(n: Long): Long = {
    if(primes.contains(n)) {
      getNotComposite(n + 2)
    } else {
      val result = isComposite(n)
      println(n + " => " + result)
      if(result.isRight) getNotComposite(n + 2)
      else result.left.get
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = getNotComposite(3)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
