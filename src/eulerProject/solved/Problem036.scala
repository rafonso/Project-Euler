package eulerProject.solved

/**
 Problem 36.<br>
31 January 2003<br>
<br>
The decimal number, 585 = 1001001001_(2) (binary), is palindromic in both bases.<br>
<br>
Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.<br>
<br>
(Please note that the palindromic number, in either base, may not include leading zeros.)<br>
 * EULER: SOLVED
 */
object Problem036 {
  
  def isPalidrome(s: String): Boolean = {
    
    def evaluate(chars: Array[Char], i: Int, j: Int): Boolean = {
      if(i >= j) {
        true
      } else if(chars(i) != chars(j)) {
        false
      } else {
        evaluate(chars, i + 1, j - 1)
      }
    }
    
    evaluate(s.toArray, 0, s.length - 1)
  }
  
  def isPalindrome(x: Long): Boolean = isPalidrome(x.toString)
  
  def sumDecimalAndBinaryPalidromes(accumulated: Long, current: Int): Long = {
    if(isPalindrome(current) && isPalidrome(current.toBinaryString)) {
      println(current + " (" + current.toBinaryString + ")")
      current + accumulated
    } else {
      accumulated
    }
  }
  
  def sumDecimalAndBinaryPalidromes(n: Long): Long = (1 until n.toInt).foldLeft(0L)(sumDecimalAndBinaryPalidromes)
  
  def main(args : Array[String]) : Unit = {
    val n = args(0).toLong
    
    val t0 = System.currentTimeMillis
    val sum = sumDecimalAndBinaryPalidromes(n)
    val deltaT = System.currentTimeMillis - t0
    
    println(sum)
    println("Time = " + deltaT + " ms")
  }
}

/*
PALINDOMS FROM 1 to 10^9
1 (1)
3 (11)
5 (101)
7 (111)
9 (1001)
33 (100001)
99 (1100011)
313 (100111001)
585 (1001001001)
717 (1011001101)
7447 (1110100010111)
9009 (10001100110001)
15351 (11101111110111)
32223 (111110111011111)
39993 (1001110000111001)
53235 (1100111111110011)
53835 (1101001001001011)
73737 (10010000000001001)
585585 (10001110111101110001)
1758571 (110101101010101101011)
1934391 (111011000010000110111)
1979791 (111100011010110001111)
3129213 (1011111011111101111101)
5071705 (10011010110001101011001)
5259525 (10100000100000100000101)
5841485 (10110010010001001001101)
13500531 (110011100000000001110011)
719848917 (101010111010000000010111010101)
910373019 (110110010000110011000010011011)
939474939 (110111111111110011111111111011) 

2609044274
Time = 613159 ms
*/