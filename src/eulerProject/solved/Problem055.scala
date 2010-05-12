package eulerProject.solved

/**
 * Problem 55: How many Lychrel numbers are there below ten-thousand? <br>
 * 24 October 2003<br>
 * <br>
 * If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.<br>
 * <br>
 * Not all numbers produce palindromes so quickly. For example,<br>
 * <br>
 * 349 + 943 = 1292,<br>
 * 1292 + 2921 = 4213<br>
 * 4213 + 3124 = 7337<br>
 * <br>
 * That is, 349 took three iterations to arrive at a palindrome.<br>
 * <br>
 * Although no one has proved it yet, it is thought that some numbers, like 
 * 196, never produce a palindrome. A number that never forms a palindrome 
 * through the reverse and add process is called a Lychrel number. Due to the 
 * theoretical nature of these numbers, and for the purpose of this problem, 
 * we shall assume that a number is Lychrel until proven otherwise. In 
 * addition you are given that for every number below ten-thousand, it will 
 * either (i) become a palindrome in less than fifty iterations, or, (ii) no 
 * one, with all the computing power that exists, has managed so far to map it 
 * to a palindrome. In fact, 10677 is the first number to be shown to require 
 * over fifty iterations before producing a palindrome: 
 * 4668731596684224866951378664 (53 iterations, 28-digits).<br>
 * <br>
 * Surprisingly, there are palindromic numbers that are themselves Lychrel 
 * numbers; the first example is 4994.<br>
 * <br>
 * <b>How many Lychrel numbers are there below ten-thousand?</b><br>
 * <br>
 * NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.<br>
 * EULER: SOLVED
 */
object Problem055 {
  
  def getInverse(n: BigInt): BigInt = {
    
    def invert(x: BigInt, inverse: BigInt): BigInt =
      if(x == 0) inverse
      else invert(x / 10, inverse * 10 + x % 10)
    
    invert(n, 0)
  }
  
  def isPalindrome(n: BigInt): Boolean = {
    
    def evaluate(chars: Array[Char], i: Int, j: Int): Boolean = 
      if(i >= j) true
      else if(chars(i) != chars(j)) false
      else evaluate(chars, i + 1, j - 1)
    
    val s = n.toString
    evaluate(s.toArray, 0, s.length - 1)
  }
  
  def isLychrel(n: BigInt, tentatives: Long): Boolean = {
    if(tentatives == 0) true
    else {
      val inverse = getInverse(n)
      val sum = n + inverse
//      print(sum + " ")
      if(isPalindrome(sum)) false
      else isLychrel(sum, tentatives - 1)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 10000
    val tentatives = 50
    val t0 = System.currentTimeMillis
    val lychrels = (1 to max).filter(isLychrel(_, tentatives))
    println("lychrels = " + lychrels)
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================")
    println("size = " + lychrels.size)
    println("Time = " + deltaT + " ms")
  }
}
