package eulerProject

/**
 * Problem 44: Find the smallest pair of pentagonal numbers whose sum and 
 * difference is pentagonal.<br/>
 * 23 May 2003<br/>
 * <br/>
 * Pentagonal numbers are generated by the formula, P(n) = n(3n-1)/2. The first 
 * ten pentagonal numbers are:<br/>
 * <br/>
 * 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...<br/>
 * <br/>
 * It can be seen that P(4) + P(7) = 22 + 70 = 92 = P(8). However, 
 * their difference, 70 - 22 = 48, is not pentagonal.<br/>
 * <br/>
 * <b>Find the pair of pentagonal numbers, P(j) and P(k), for which their 
 * sum and difference is pentagonal and D = |P(k) - P(j)| is minimised; 
 * what is the value of D?<b><br/>
 * 
 */
object Problem044 {
  
  def isPentagonal(p: Long): Boolean = {
    val deltaRoot = Utils.getSqrt(1 + 24 * p)
    if(deltaRoot.isRight) {
      val num = 1 + deltaRoot.right.get
      (num % 6 == 0) 
    } else {
      false
    }
  }
  
  def getD: ((Long, Long), (Long, Long)) = {
    
    import scala.collection.immutable._
    
    def evalNAndPriors(j: Int, pentagonals: Map[Long, Long], k: Long, pk: Long): Option[((Long, Long), (Long, Long))] = {
      if(j >= pentagonals.size) {
        None
      } else {
        val pj = pentagonals(j)
        val diff = pk - pj
        if(pentagonals.valuesIterator.contains(diff)) {
          val sum = pk + pj
          if(isPentagonal(sum)) Some(((j, pj), (k, pk)))
          else evalNAndPriors(j + 1, pentagonals, k, pk)
        } else {
          evalNAndPriors(j + 1, pentagonals, k, pk)
        }
      }
    }
    
    def eval(n: Long, pentagonals: Map[Long, Long]): ((Long, Long), (Long, Long)) = {
      val isPresent = pentagonals.get(n).isDefined
      val pn = (n * (3 * n - 1) / 2)
      println("p(%5d) = %8d".format(n, pn))
      val result = evalNAndPriors(1, pentagonals, n, pn)
      if(result.isDefined) result.get
      else eval(n + 1, pentagonals + (n -> pn))
    }
    
    eval(1, TreeMap.empty[Long, Long])
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = getD
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
