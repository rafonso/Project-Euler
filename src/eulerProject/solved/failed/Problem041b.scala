package eulerProject

object Problem041b {
  
  def getPermutations(digits: List[Int]): List[Long] = {
    
    val permutations = new scala.collection.mutable.ListBuffer[Long]()
    
    def mkPermutation(myDigits: List[Int], value: Long): Unit = myDigits match {
      case Nil => permutations + value
      case _ => myDigits.foreach(d => mkPermutation(myDigits.remove(_ == d), value * 10 + d))
    }
    
    mkPermutation(digits, 0)
    permutations.toList.sort(_ < _)
  }
  
  def isPrime(n: Long, primes: List[Long]): Boolean = primes match {
    case Nil => true
    case prime :: rest if(n % prime == 0) => false 
    case _ :: rest => isPrime(n, rest)
  }
  
  def getGreatestPrime(numbers: List[Long], primes: List[Long], currentPrime: Long): Long = {
      println(numbers.head)
      numbers match {
      case Nil => currentPrime
      case number :: rest if(isPrime(number, primes)) => getGreatestPrime(rest, primes, number)
      case _ :: rest => getGreatestPrime(rest, primes, currentPrime)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val permutations = getPermutations((0 until 10).toList) 
    println(permutations.size)
    
    val primes: List[Long] = Utils.getPrimesUntil(Math.sqrt(9876543210L).toInt) 
    println(primes.size)
    println(getGreatestPrime(permutations, primes, 0))
  }
}
