package eulerProject.solved

/**
 * Problem 25: What is the first term in the Fibonacci sequence to contain 1000 digits?<br>
 * 30 August 2002<br>
 * <br>
 * The Fibonacci sequence is defined by the recurrence relation:<br>
 * <br>
 * F(n) = F(n - 1) + F(n - 2), where F(1) = 1 and F(2) = 1.<br>
 * <br>
 * Hence the first 12 terms will be:<br>
 * <br>
 * F(1) = 1<br>
 * F(2) = 1<br>
 * F(3) = 2<br>
 * F(4) = 3<br>
 * F(5) = 5<br>
 * F(6) = 8<br>
 * F(7) = 13<br>
 * F(8) = 21<br>
 * F(9) = 34<br>
 * F(10) = 55<br>
 * F(11) = 89<br>
 * F(12) = 144<br>
 * <br>
 * The 12th term, F(12), is the first term to contain three digits.<br>
 * <br>
 * <b>What is the first term in the Fibonacci sequence to contain 1000 digits?<b><br>
 * <br>
 * EULER: SOLVED
 */
object Problem025 {
  
  def getDigitsQuantity(n: BigInt): Int = n.toString.length
  
  def getFibonacciUntilSize(size: Int): Int = {
    
    def calculate(prePreValue: BigInt, preValue: BigInt, term: Int): Int = {
      val value = prePreValue + preValue
      if(getDigitsQuantity(value) >= size) {
        term
      } else {
        calculate(preValue, value, term + 1)
      }
    } 
      
    calculate(1, 1, 3)
  }
  
  def main(args : Array[String]) : Unit = {
    val size = args(0).toInt
    
    val t0 = System.currentTimeMillis
    val result = getFibonacciUntilSize(size)
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================================================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
