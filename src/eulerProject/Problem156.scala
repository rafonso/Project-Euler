package eulerProject

/** 
 * Problem 156: Counting Digits<br>
 * 25 May 2007<br>
 * <br>
 * Starting from zero the natural numbers are written down in base 10 like 
 * this:<br>
 * 0 1 2 3 4 5 6 7 8 9 10 11 12....<br>
 * <br>
 * Consider the digit d=1. After we write down each number n, we will update 
 * the number of ones that have occurred and call this number f(n,1). The 
 * first values for f(n,1), then, are as follows:<br>
 * <table align="center">
 * <tr>
 * <td>n</td>	<td>f(n,1)</td>
 * </tr>
 * <tr>
 * <td>0</td>	<td>0</td>
 * </tr>
 * <tr>
 * <td>1</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>2</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>3</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>4</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>5</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>6</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>7</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>8</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>9</td>	<td>1</td>
 * </tr>
 * <tr>
 * <td>10</td>	<td>2</td>
 * </tr>
 * <tr>
 * <td>11</td>	<td>4</td>
 * </tr>
 * <tr>
 * <td>12</td>	<td>5</td>
 * </tr>
 * </table>
 * <br>
 * Note that f(n,1) never equals 3.<br>
 * So the first two solutions of the equation f(n,1)=n are n=0 and n=1. The 
 * next solution is n = 199.981.<br>
 * <br>
 * In the same manner the function f(n,d) gives the total number of digits d 
 * that have been written down after the number n has been written.<br>
 * In fact, for every digit d != 0, 0 is the first solution of the equation 
 * f(n,d)=n.<br>
 * <br>
 * Let s(d) be the sum of all the solutions for which f(n,d)=n.<br>
 * You are given that s(1) = 22.786.974.071.<br>
 * <br>
 * <b>Find Sum(s(d)) for 1 <= d <= 9.</b><br>
 * <br>
 * Note: if, for some n, f(n,d)=n for more than one value of d this value of n 
 * is counted again for every value of d for which f(n,d)=n.<br>
 * 
 */
object Problem156 {
  
  def countDigit(n: Long, digit: Int): Long = {
    
    def calculate(x: Long, sum: Long): Long = {
      if(x == 0) sum
      else {
        val lastDigit = x % 10
        if(lastDigit == digit) calculate(x / 10, sum + 1)
        else calculate(x / 10, sum)
      }
    }
    
    calculate(n, 0L)
  }
  
  class SumDigitIterator(val digit: Int, val canContinue: (Int, Long, Long) => Boolean) extends BufferedIterator[(Long, Long)] {
    
    private var n = 0L
    
    private var sum = 0L
    
    def head = (this.n, this.sum)
    
    def hasNext = canContinue(this.digit, this.n, this.sum)
    
    def next = {
      this.n += 1
      this.sum += countDigit(this.n, this.digit)
      (this.n, this.sum)
    }
  }
  
  def f(n: Int, d: Int, sum: Long, stopCondition: (Int, Int, Long) => Boolean): Long = 
    if(stopCondition(n, d, sum)) sum
    else f(n + 1, d, countDigit(n + 1, d) + sum, stopCondition)
  
  def showStopN(d: Int, n: Long, sum: Long) = {
//    println("f(" + n + ", " + d + ") = " + sum)
    n <= 2000000000
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val iterator = new SumDigitIterator(1, showStopN)
    var sum = 0L
    while(iterator.hasNext) {
      val tuple = iterator.next
      if(tuple._1 == tuple._2) {
        println(tuple._1)
        sum += tuple._1
      }
    }
    val deltaT = System.currentTimeMillis - t0
    
    println("SUM = " + sum)
    println("Time = " + deltaT + " ms")
  }
}
