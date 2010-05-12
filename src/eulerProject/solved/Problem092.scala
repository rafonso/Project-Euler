package eulerProject.solved

/**
 * Problem 92: Investigating a square digits number chain with a surprising 
 * property.<br>
 * 01 April 2005<br>
 * <br>
 * A number chain is created by continuously adding the square of the digits 
 * in a number to form a new number until it has been seen before.<br>
 * <br>
 * For example,<br>
 * <br>
 * 44 -> 32 -> 13 -> 10 -> 1 -> 1<br>
 * 85 -> 89 -> 145 -> 42 -> 20 -> 4 -> 16 -> 37 -> 58 -> 89<br>
 * <br>
 * Therefore any chain that arrives at 1 or 89 will become stuck in an 
 * endless loop. What is most amazing is that EVERY starting number will 
 * eventually arrive at 1 or 89.<br>
 * <br>
 * <b>How many starting numbers below ten million will arrive at 89?</b><br>
 */
object Problem092 {
  
  val squares = Array(0, 1, 4, 9, 16, 25, 36, 49, 64, 81)
  
  def getDigitsSquare(x: Int): Int = {
    
    def eval(n: Int, sum: Int): Int = n match {
      case 0 => sum
      case _ => {
        val (div, digit) = Utils./%(n, 10)
        eval(div, sum + squares(digit))
      }
    }
    
    eval(x, 0)
  }
  
//  import scala.collection.Set
    
  def getNumbers89Until(limit: Int): Int = {
    
    def verify(current: Int, numbers89: Set[Int], numbers1: Set[Int], accumulatedNumbers: List[Int]): (Boolean, List[Int]) = {
      if(numbers89(current)) (true, accumulatedNumbers.filter(_ <= limit))
      else if(numbers1(current)) (false, accumulatedNumbers.filter(_ <= limit))
      else verify(getDigitsSquare(current), numbers89, numbers1, current :: accumulatedNumbers)
    }
    
    def evalN(n: Int, numbers89: Set[Int], numbers1: Set[Int]): Set[Int] = {
      if(n > limit) numbers89
      else if(numbers89(n) || numbers1(n)) evalN(n + 1, numbers89, numbers1)
      else {
        val (is89, sequence) = verify(n, numbers89, numbers1, Nil)
//        if(n % 40 == 0) println("%5s => %s".format(is89, sequence.reverse))
        if(is89) evalN(n + 1, numbers89 ++ sequence, numbers1)
        else evalN(n + 1, numbers89, numbers1 ++ sequence)
      }
    }
    
    evalN(2, Set(89), Set(1)).size
  }  
  
  def main(args : Array[String]) : Unit = {
    val max = 10000000
    
    val t0 = System.currentTimeMillis
    val result = getNumbers89Until(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
