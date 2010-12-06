package eulerProject.solved

/**
 * Problem 49: Find arithmetic sequences, made of prime terms, whose four 
 * digits are permutations of each other.<br>
 * 01 August 2003<br>
 * <br>
 * The arithmetic sequence, 1487, 4817, 8147, in which each of the terms 
 * increases by 3330, is unusual in two ways: (i) each of the three terms are 
 * prime, and, (ii) each of the 4-digit numbers are permutations of one another.<br>
 * <br>
 * There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
 * exhibiting this property, but there is one other 4-digit increasing sequence.<br>
 * <br>
 * <b>What 12-digit number do you form by concatenating the three terms in this 
 * sequence?</b><br>
 * 
 */
object Problem049a {
  
  def normalizeNumber(n: Long) = n.toString.toList.sort(_ < _).mkString
  
  def getPalindromesFor(n: Long, numbers: List[Long]): List[Long] = {
    val nString = normalizeNumber(n)
    for(x <- numbers; if (normalizeNumber(x) == nString)) yield x
  }
  
  def getPalindromes(numbers: List[Long]): List[List[Long]] = {
    
    def eval(nums: List[Long], palindromes: List[List[Long]]): List[List[Long]] = nums match {
      case Nil => palindromes.reverse
      case num :: others => {
        val pal = getPalindromesFor(num, others)
        pal match {
          case Nil => eval(others, palindromes)
          case _ => eval(others -- pal, (num :: pal) :: palindromes)
        }
      }
    }
    
    eval(numbers, Nil)
  }
  
  type trio = Option[(Long, Long, Long)]
  
  def hasArithmeticSequence(numbers: List[Long]): trio = {
    
    def eval2(n: Long, longs: List[Long]): trio = longs match {
      case List(n) => None
      case first :: others => {
        val diff = first - n
        val next = first + diff
        if(others.contains(next)) Some((n, first, next))
        else if(next > others.last) None
        else eval2(n, others)
      }
    }
    
    def eval(nums: List[Long]): trio = nums match {
      case List(n1, n2) => None
      case n :: others => {
        val trio1 = eval2(n, others)
        if(trio1.isDefined) trio1
        else eval(others)
      }
    }
    
    eval(numbers)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    println("Getting Primes")
    val myPrimes = eulerProject.Utils.getPrimesUntil(10000L).filter(_ > 1000)
    println("Getting Palindromes")
    val palindromes = getPalindromes(myPrimes).filter(_.size >= 3)
    println("Getting Our Palindromes")
    val myPalindromes = palindromes
      .map(hasArithmeticSequence(_))
      .filter(_.isDefined)
      .map(_.get)
    val deltaT = System.currentTimeMillis - t0
    
    println("========================")
//    println(palindromes)
    println(myPalindromes)
    println("Time = " + deltaT + " ms")
    println(hasArithmeticSequence(List(1033, 1303, 3301)))
  }
}
