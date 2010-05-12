package eulerProject.solved

/**
 * Problem 23: Find the sum of all the positive integers which cannot be written 
 * as the sum of two abundant numbers.<br>
 * 02 August 2002<br>
 * <br>
 * A perfect number is a number for which the sum of its proper divisors is 
 * exactly equal to the number. For example, the sum of the proper divisors of 
 * 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect 
 * number.<br>
 * <br>
 * A number n is called deficient if the sum of its proper divisors is less than 
 * n and it is called abundant if this sum exceeds n.<br>
 * <br>
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest 
 * number that can be written as the sum of two abundant numbers is 24. By 
 * mathematical analysis, it can be shown that all integers greater than 28123 
 * can be written as the sum of two abundant numbers. However, this upper limit 
 * cannot be reduced any further by analysis even though it is known that the 
 * greatest number that cannot be expressed as the sum of two abundant numbers 
 * is less than this limit.<br>
 * <br>
 * Find the sum of all the positive integers which cannot be written as the sum 
 * of two abundant numbers.<br>
 * EULER: SOLVED
 */
object Problem023 {
  
  def getDivisors(n: Int): List[Int] = {
    val divisors = new scala.collection.mutable.ListBuffer[Int]()
    
    (1 to (n / 2)).foreach(i =>
        if(n % i == 0) {
            divisors + i
        }
    )
   
    divisors.toList
  }

  def isAbundant(n: Int): Boolean = {
    val divisors = getDivisors(n)
    val sum = divisors.foldLeft(0)(_ + _)
    (sum > n)
  }
  
  def getNotAmbudantSum(abudants: Array[Int], max: Int) : List[Int] = {
    
    val abumdantsSum = new Array[Boolean](max + 1)
    
    (0 until abudants.length).foreach(i => {
      (i until abudants.length).foreach(j => {
        val sum = abudants(i) + abudants(j)
        if(sum <= max) {
          abumdantsSum(sum) = true 
        }
      })
    })
    
    def accumulate(i: Int): List[Int] = {
      if(i >= max) {
        Nil
      } else if(abumdantsSum(i)) {
        accumulate(i + 1)
      } else {
        i :: accumulate(i + 1)
      }
    }
    

    accumulate(0)
  }
  
  def main(args : Array[String]) : Unit = {
    val MAX = 28123
    println("Getting divisors until " + MAX)
    
    val t0 = System.currentTimeMillis
    val ambudants = (1 to MAX).filter(isAbundant(_)).toArray
    val notSumAbundants = getNotAmbudantSum(ambudants, MAX)
    val sum = notSumAbundants.foldLeft(0)(_ + _)
    val deltaT = System.currentTimeMillis - t0

    println("Total Time: " + deltaT + " ms")
    println(ambudants.size)
    println(notSumAbundants.size)
    println(sum)
//    println(perfects.toArray)
  }
}
