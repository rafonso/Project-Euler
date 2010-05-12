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
 * 28 = 2^2 + 2^3 + 2^4<br/>
 * 33 = 3^2 + 2^3 + 2^4<br/>
 * 49 = 5^2 + 2^3 + 2^4<br/>
 * 47 = 2^2 + 3^3 + 2^4<br/>
 * <br/>
 * <b>How many numbers below fifty million can be expressed as the sum of a prime 
 * square, prime cube, and prime fourth power?</b><br/>
 * <br/>
 */
object Problem087 {
  
  def getSums(maxValue: Long): Int = {
    
    def getPowers(bases: List[Long], exp: (Long) => Long): List[Long] = bases.map(exp(_)).takeWhile(_ < maxValue)
    
    def iterateFourths(square: Long, cube: Long, fourths: List[Long], size: Int): Int = fourths match {
      case Nil => size
      case f :: fs => {
        val sum = square + cube + f
//        println("%,8d + %,8d + %,8d = %,8d".format(square, cube, f, sum))
        if(sum > maxValue) size
        else iterateFourths(square, cube, fs, size + 1)
      }
    }
    
    def iterateCubes(square: Long, cubes: List[Long], fourths: List[Long], size: Int): Int = cubes match {
      case Nil => size
      case cube :: cs => iterateCubes(square, cs, fourths, iterateFourths(square, cube, fourths, 0) + size)
    }
    
    def iterateSquares(squares: List[Long], cubes: List[Long], fourths: List[Long], size: Int): Int = squares match {
      case Nil => size
      case square :: ss => {
        println("%,10d".format(square))
        iterateSquares(ss, cubes, fourths, iterateCubes(square, cubes, fourths, 0) + size)
      }
    }
    
    val maxSqrt = Math.sqrt(maxValue).toLong + 1
    println("getting primes until " + maxSqrt)
    val primes = Utils.getPrimesUntil(maxSqrt.toLong)
    println("There are " + primes.size + " primes until " + maxSqrt)
    print("Getting squares: ")
    val squares = getPowers(primes, x => x * x)
    println(squares.size)
    print("Getting cubes:   ")
    val cubes = getPowers(primes, x => x * x * x)
    println(cubes.size)
    print("Getting fourths: ")
    val fourths = getPowers(primes, x => x * x * x * x)
    println(fourths.size)
    
    iterateSquares(squares, cubes, fourths, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 50000000
    
    val t0 = System.currentTimeMillis
    val result = getSums(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
//    result.foreach(println(_))
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
/*
==================
1139575
Time = 225968 ms
*/