package eulerProject

/**
 * Problem 268: Counting numbers with at least four distinct prime factors 
 * less than 100<br>
 * 11 December 2009<br>
 * <br>
 * It can be verified that there are 23 positive integers less than 1000 that 
 * are divisible by at least four distinct primes less than 100.<br>
 * <br>
 * <b>Find how many positive integers less than 10^(16) are divisible by at 
 * least four distinct primes less than 100.</b><br>
 * <br>
 */
object Problem268 {
  
  import scala.collection.immutable._
  
  val primes:List[Long] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
  
  def multiply(numbers: Seq[Long]): BigInt = numbers.foldLeft(BigInt(1))(_ * _)
  
  def getProduct(maxValue: Int): Set[Int] = {
    
    
    
    Set.empty
  }
  
  def run(max: Long): Unit = {
    
    def next(l: Long): Unit = {
      if(l < max) {
        if(l % 10000 == 0) println(l)
        next(l + 1)
      }
    }
    
    next(1L)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = (1 to 10).foldLeft(1L)((acc, l) => acc * 10)
    println(max)
    run(max)
    /*
    val primes2 = new TreeSet[Long] ++ primes.flatMap(
      p1 => primes
        .filter(_ != p1)
        .map(_ * p1)
    )
    println(primes2)
    val primes3 = new TreeSet[Long] ++ primes.flatMap(
      p1 => primes.filter(_ != p1).flatMap(
        p2 => primes
          .filter(p3 => (p3 != p1) && (p3 != p2))
          .map(p1 * p2 * _)
      )
    )
    println(primes3)
    val primes4 = new TreeSet[Long] ++ primes.flatMap(
      p1 => primes.filter(_ != p1).flatMap(
        p2 => primes
          .filter(p3 => (p3 != p1) && (p3 != p2)).flatMap(
          p3 => primes
            .filter(p4 => (p4 != p1) && (p4 != p2) && (p4 != p3))
            .map(p1 * p2 * p3 * _)
        )          
      )
    )
    println(primes4)
    
    val totalPrimes = primes ++ primes2 ++ primes3 ++ primes4
    println(totalPrimes)
    println(totalPrimes.size)
    */
  }
}