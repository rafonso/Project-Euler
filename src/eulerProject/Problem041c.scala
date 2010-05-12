package eulerProject

/**
 * Problem 41: What is the largest n-digit pandigital prime that exists?<br>
 * 11 April 2003<br>
 * <br>
 * We shall say that an n-digit number is pandigital if it makes use of all 
 * the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
 * and is also prime.<br>
 * <br>
 * <b>What is the largest n-digit pandigital prime that exists?</b><br>
 * 
 */
object Problem041c {
  
  def removeDigit(d: Int, digits: List[Int]) = digits.remove(_ == d)
  
  def getGreatestPrimeForDigits(digits: List[Int], primes: List[Long], greatestPrime: Long): Long = {
    
    def eval(currentDigits: List[Int], currentValue: Long, greatest: Long): Long = currentDigits match {
      case Nil if(currentValue <= greatest) => greatest
      case Nil if(currentValue >  greatest) => {
        if(currentValue < greatest) {
          greatest
        } else if(primes.takeWhile(_ <= Math.sqrt(currentValue)).exists(currentValue % _ == 0)) {
          greatest
        } else {
          println(currentValue)
          currentValue
        }
      }
      case _ => currentDigits.foldLeft(greatest)((g, d) => eval(removeDigit(d, currentDigits), currentValue * 10 + d, g))
    }
    
    eval(digits, 0, greatestPrime)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 31680L
    val primes = Utils.getPrimesUntil(max)
    println("There are " + primes.size + " primes until " + max + ". Last is " + primes.last)
    println(primes.last * primes.last)
    val digits = (9 to 0 by -1).toList
    
    val t0 = System.currentTimeMillis
    val result = digits.foldLeft(0L){(g, d) => getGreatestPrimeForDigits(removeDigit(d, digits), primes, g)}
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
  
  /**
      digits.foldLeft(0L){(g, d) =>
      getGreatestPrimeForDigits(removeDigit(d, digits), primes, g)
    }
      val digitsWithoutD = removeDigit(d, digits)
      digitsWithoutD.foldLeft(g){(g1, d1) =>
        getGreatestPrimeForDigits(removeDigit(d1, digitsWithoutD), primes, g1)
      }
   */
}
