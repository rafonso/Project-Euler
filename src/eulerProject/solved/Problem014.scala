package eulerProject.solved

/**
 * Problem 14: Find the longest sequence using a starting number under one million.<br>
 * 05 April 2002<br>
 * <br>
 * The following iterative sequence is defined for the set of positive integers:<br>
 * <br>
 * n -> n/2 (n is even)<br>
 * n -> 3n + 1 (n is odd)<br>
 * <br>
 * Using the rule above and starting with 13, we generate the following sequence:<br>
 * 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1<br>
 * <br>
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 
 * 10 terms. Although it has not been proved yet (Collatz Problem), it is 
 * thought that all starting numbers finish at 1.<br>
 * <br>
 * <b>Which starting number, under one million, produces the longest chain?</b><br>
 * <br>
 * NOTE: Once the chain starts the terms are allowed to go above one million.<br>
 * <br>
 * EULER: SOLVED
 */
object Problem014 {
  
  def isEven(n: Long) = (n % 2 == 0)
  
  def getEvenTerm(n: Long) = n / 2
  
  def getOddTerm(n: Long) = 3 * n + 1
  
  /*
  
  def sequencia14(n: Int, sequence: List[Int]): List[Int] = {
//    print(n + " ")
    n match {
      case 1 => sequence.reverse
      case x if(isEven(x)) => sequencia14(getEvenTerm(n), getEvenTerm(n) :: sequence)
      case _ => sequencia14(getOddTerm(n), getOddTerm(n) :: sequence)
    }
  }
  
  def sequencia14(n: Int): List[Int] = sequencia14(n, List(n))
  
  */
  /*
  def sizeSequence(n: Long, size:Int) : Int = n match {
    case 1 => size
    case x if(isEven(x)) => sizeSequence(getEvenTerm(n), size + 1)
    case _ => sizeSequence(getOddTerm(n), size + 1)
  }
  
  def sizeSequence(n: Int) : Int = sizeSequence(n, 1)

  def getGreatestSize(iSize: (Int, Long), i: Int, max: Int): (Int, Long) = {
    if(i > max) {
      iSize
    } else {
      val size = sizeSequence(i)
      if(size > iSize._2) {
        printf("size(%,7d) = %,d %n", i, size)
        getGreatestSize((i, size), i + 1, max)
      } else {
        getGreatestSize(iSize, i + 1, max)
      }
    }
  }
  
  def getGreatestSize(max: Int): (Int, Long) = getGreatestSize((0, 0), 1, max)
  */
  
  val sizeByValue = new scala.collection.mutable.HashMap[Long, Long]()
  sizeByValue += (1L -> 1L)
  
  def calculateSequenceLength(n: Long): Long = {
    val optionSize = sizeByValue.get(n)
    if(optionSize.isDefined) {
      optionSize.get
    } else {
      val nextTerm = if(isEven(n)) getEvenTerm(n) else getOddTerm(n)
      1 + calculateSequenceLength(nextTerm)
    }
  }
  
  def getTermWithGreatestLength(max: Long): (Long, Long) = {
    
    def evaluate(i: Int, maxSize: (Long, Long)) : (Long, Long) = {
      if(i > max) {
        maxSize
      } else {
        val currentSize = calculateSequenceLength(i)
        sizeByValue.put(i, currentSize)
        if(currentSize > maxSize._2)  {
          printf("size(%,7d) = %,d %n", i, currentSize)
          evaluate(i + 1, (i, currentSize))
        } else { 
          evaluate(i + 1, maxSize)
        }
      }
    }
    
    evaluate(1, (0, 0))
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000000L
    
    val t0 = System.currentTimeMillis
    val result = getTermWithGreatestLength(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================================================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
