package eulerProject.solved.failed

import scala.collection.mutable.ListBuffer

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
object Problem049 {
  
  
  def getPrimesFromTo(from: Long, to: Long): List[Long] = {
      
    val primeIterator = new eulerProject.PrimeIterator
    val p = primeIterator.iterateUntil(from)
    val myPrimes = new ListBuffer[Long]
    while(primeIterator.next < to) myPrimes += primeIterator.head
    myPrimes.toList
  }
  
  def normalizeNumber(n: Long) = new String(n.toString.toList.sort(_ < _).toArray)
  
  def isPalindrome(n: Long, base: String): Boolean = {
    val str1 = n.toString
    val lst = str1.toList
    val lst1 = lst.sort(_ < _)
    val arr = lst1.toArray
    
    val str = new String(arr)
      //new String(n.toString.toList.sort(_ < _).toArray)
    (base == str)
  }
  
  def getPalindromesFor(n: Long, numbers: List[Long]): List[Long] = {
    val nString = normalizeNumber(n)
    for(x <- numbers; if (normalizeNumber(x) ==  nString)) yield x
  }
  
  def getPalindromes(numbers: List[Long]): List[List[Long]] = {
    
    def eval(nums: List[Long], palindromes: List[List[Long]]): List[List[Long]] = nums match {
      case Nil => palindromes
      case x :: xs => {
        val pal = getPalindromesFor(x, nums)
        if(pal.size > 1) eval(nums -- pal, palindromes ::: List(pal))
        else eval(nums -- pal, palindromes)
      }
    }
    
    eval(numbers, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val min = 1000L
    val max = 10000L
    
//    val n = 123
    val l: List[Long] = List(1487, 2512, 4817, 5632, 8417, 9001)
    
    val t0 = System.currentTimeMillis
    val myPrimes = getPrimesFromTo(min, max)
    val pal = getPalindromes(myPrimes).filter(_.size > 2)
    val deltaT = System.currentTimeMillis - t0
    
    println("========================")
//    println(myPrimes)
    pal.foreach(println(_))
    println(pal.size)
    println("Time = " + deltaT + " ms")
  }
}
