package eulerProject

/**
 * Problem 74: Determine the number of factorial chains that contain exactly 
 * sixty non-repeating terms.<br>
 * 16 July 2004<br>
 * <br>
 * The number 145 is well known for the property that the sum of the factorial 
 * of its digits is equal to 145:<br>
 * <br>
 * 1! + 4! + 5! = 1 + 24 + 120 = 145<br>
 * <br>
 * Perhaps less well known is 169, in that it produces the longest chain of 
 * numbers that link back to 169; it turns out that there are only three such 
 * loops that exist:<br>
 * <br>
 * 169 -> 363601 -> 1454 -> 169<br>
 * 871 -> 45361 -> 871<br>
 * 872 -> 45362 -> 872<br>
 * <br>
 * It is not difficult to prove that EVERY starting number will eventually get 
 * stuck in a loop. For example,<br>
 * <br>
 * 69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)<br>
 * 78 -> 45360 -> 871 -> 45361 (-> 871)<br>
 * 540 -> 145 (-> 145)<br>
 * <br>
 * Starting with 69 produces a chain of five non-repeating terms, but the 
 * longest non-repeating chain with a starting number below one million is 
 * sixty terms.<br>
 * <br>
 * <b>How many chains, with a starting number below one million, contain 
 * exactly sixty non-repeating terms?</b><br>
 * <br>
 */
object Problem074 {
  
  val digitsFactorial = Array(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
  
  def getNextTerm(n: Long): Long = {
    
    def sumFactorialDigits(x: Long, sum: Long): Long = 
      if(x == 0) sum
      else sumFactorialDigits(x / 10, digitsFactorial((x % 10).intValue) + sum)
  
    sumFactorialDigits(n, 0)
  }
  
  def getSequenceFor(n: Long): List[Long] = {
    
    def makeSequence(x: Long, sequence: List[Long]): List[Long] = {
      val term = getNextTerm(x)
      if(sequence.contains(term)) sequence.reverse
      else makeSequence(term, term :: sequence)
    }
    
    makeSequence(n, List(n))
  }
  
  def getQuantityOfSequencesWithSizeUntil(size: Int, max: Int): Int = {
    
    def evaluate(n: Int, quantityAccumulated: Int): Int = {
      if(n > max) quantityAccumulated
      else {
        val sequence = getSequenceFor(n)
        if(sequence.size >= size) {
        println(n + " (" + sequence.size + ") => " + sequence)
          evaluate(n + 1, quantityAccumulated + 1)
        } else {
          evaluate(n + 1, quantityAccumulated)
        }
      }
    }
    
    evaluate(1, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000000
    val size = 60
    
    val t0 = System.currentTimeMillis
    val result = getQuantityOfSequencesWithSizeUntil(size, max)
    val deltaT = System.currentTimeMillis - t0
    
    println("========================")
    println(result)
    println("TIME: " + deltaT + " ms")
  }
}
