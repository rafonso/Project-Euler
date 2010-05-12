package eulerProject

/**
 * Problem 87: Investigating numbers that can be expressed as the sum of a 
 * prime square, cube, and fourth power?<br/>
 * 21 January 2005<br/>
 * <br/>
 * The smallest number expressible as the sum of a prime square, prime cube, 
 * and prime fourth power is 28. In fact, there are exactly four numbers below 
 * fifty that can be expressed in such a way:<br/>
 * <br/>
 * 28 = 2<sup>2</sup> + 2<sup>3</sup> + 2<sup>4</sup><br/>
 * 33 = 3<sup>2</sup> + 2<sup>3</sup> + 2<sup>4</sup><br/>
 * 49 = 5<sup>2</sup> + 2<sup>3</sup> + 2<sup>4</sup><br/>
 * 47 = 2<sup>2</sup> + 3<sup>3</sup> + 2<sup>4</sup><br/>
 * <br/>
 * <b>How many numbers below fifty million can be expressed as the sum of a prime 
 * square, prime cube, and prime fourth power?</b><br/>
 * <br/>
 */
object Problem087a {
  
  import Utils._
  
  /**
   * 2^2 + 2^3 + x^4 = 4 + 8 + x^4  = 12 + x^4 = max <--> x = sqrt(sqrt(max - 12))
   * x^2 + 2^3 + 2^4 = x^2 + 8 + 16 = x^2 + 24 = max <--> x = sqrt(max - 24)
   */
  def getPrimesPowers(max: Long): (List[Long], List[Long], List[Long]) = {
    val limit = Math.sqrt(max).toLong
    val arrays =  getPrimesUntil(limit).map(p => Array(1L, p, p * p, p * p * p, p * p * p * p))
    
    val squares = arrays.map(_(2)).toList
    val cubes   = arrays.map(_(3)).filter(_ <= max).toList
    val fourths = arrays.map(_(4)).filter(_ <= max).toList
    
    (squares, cubes, fourths)
  }
  
  def getNumbersUntil(max: Int): Int = {
    
    def getSum(square: Long, cube: Long, fourth: Long): Long = {
      val sum = square + cube + fourth
      if(sum < max) println("%,10d + %,10d + %,10d = %,10d".format(square, cube, fourth, sum))
      sum
    }
    
    def evalSquareCubeFourth(square: Long, cube: Long, fourths: List[Long]) =
      fourths.filter(_ <= max).map(fourth => square + cube + fourth).takeWhile(_ <= max).size
    
    def evalSquareCube(square: Long, cubes: List[Long], fourths: List[Long]) = 
      cubes.filter(_ <= max).foldLeft(0)((sum, cube) => sum + evalSquareCubeFourth(square, cube, fourths))
    
    val (squares, cubes, fourths) = getPrimesPowers(max)
    squares.foldLeft(0)((sum, square) => sum + evalSquareCube(square, cubes, fourths))
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 50000000
    
    val t0 = System.currentTimeMillis
    val result = getNumbersUntil(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
