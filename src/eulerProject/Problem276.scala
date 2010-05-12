package eulerProject

/**
 * Problem 276: Primitive Triangles<br/>
 * 29 January 2010<br/>
 * <br/>
 * Consider the triangles with integer sides a, b and c with a <= b <= c.<br/>
 * <br/>
 * An integer sided triangle (a,b,c) is called primitive if gcd(a,b,c)=1.<br/>
 * <br/>
 * <b>How many primitive integer sided triangles exist with a perimeter not 
 * exceeding 10 000 000?</b><br/>
 * 
 */
object Problem276 {
  
  type Triangle = (Int, Int, Int)
  
  def getCommonDivisor(primes: List[Long], numbers: Seq[Int]): Long = {
    
    val limit = numbers(0) / 2
    
    def evaluatePrime(currentPrimes: List[Long]): Long = currentPrimes match {
      case p :: others if(p > limit) => 1
      case p :: others if(numbers.forall(_ % p == 0)) => p
      case p :: others => evaluatePrime(others)
      case Nil => error("I don't have more primes for evaluate. Numbers: " + numbers.toString)
    }
    
    evaluatePrime(primes)
  }
  
  def getTrianglesForPerimeter(perimeter: Int, primes: List[Long]): List[Triangle] = {
    
    val limit = perimeter / 3
    
    def evaluateAB(a: Int, b: Int): Option[Triangle] = {
      val c = perimeter - a - b
//      println(a + ", " + b + ", " + c)
      if(c >= (a + b)) None
      else if(getCommonDivisor(primes, Array(a, b, c)) == 1) Some((a, b, c)) 
      else None
    }
    
    def evaluateA(a: Int, triangles: List[Triangle]): List[Triangle] = {
      if(a <= limit) {
//        val trianglesA = for(b <- (a to (perimeter - a) / 2); triangle = evaluateAB(a, b); if(triangle.isDefined)) yield triangle.get
        val start = if(a < (perimeter / 4)) (perimeter / 2 - a) else a
        val end = ((perimeter - a) / 2)
        val trianglesA = (start to end)
          .map(evaluateAB(a, _))
          .filter(_.isDefined)
          .map(_.get)
        evaluateA(a + 1,  triangles ::: trianglesA.toList)
      } else {
        triangles
      }
    }
    
    evaluateA(2, Nil)
  }
  
  def countPrimitiveTriangles(maxPerimeter: Int): Int = {
    
    val primes: List[Long] = Utils.getPrimesUntil(maxPerimeter)
    
    def evaluatePerimeter(perimeter: Int, quantity: Int): Int = {
      if(perimeter <= maxPerimeter) {
        val triangles = getTrianglesForPerimeter(perimeter, primes)
//        println("Triangles for %,9d (%,9d): %s".format(perimeter, triangles.size, triangles))
        println("Triangles for %,9d : %,9d".format(perimeter, triangles.size))
        evaluatePerimeter(perimeter + 1, quantity + triangles.size)
      } else {
        quantity
      }
    }
    
    evaluatePerimeter(2, 0)
  }
  
  def getDivisors(max: Int): List[List[Long]] = {
    
    import scala.collection.immutable.Set._
    
    val primes: List[Long] = Utils.getPrimesUntil(max)
    val setPrimes = Set.empty[Long] ++ primes
    val listOne = List(1L)
    
    def getDivisorsForN(n: Int): List[Long] = {
      val limit = n / 2
      
      def generate(currentPrimes: List[Long], divisors: List[Long]): List[Long] = currentPrimes match {
        case p :: others if(p > limit) => 1 :: (n.toLong :: divisors).reverse
        case p :: others if(n % p == 0) => generate(others, p :: divisors)
        case p :: others if(n % p != 0) => generate(others, divisors)
        case _ => error("I don't have more primes for evaluate. n: " + n)
      }
      
      generate(primes, Nil)
    }
    
    def generateDivisors(n: Int, divisors: List[List[Long]]): List[List[Long]] = {
      if(n <= max) {
        val divisorsN = if(setPrimes(n)) List(n.toLong) else getDivisorsForN(n)
        generateDivisors(n + 1, divisorsN :: divisors)
      } else {
        divisors.reverse
      }
    }
    
    generateDivisors(0, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 10000
    
    val t0 = System.currentTimeMillis
    val result = countPrimitiveTriangles(max)
      //countPrimitiveTriangles(max)
      //getDivisors(max)
      //getTrianglesForPerimeter(max, Utils.getPrimesUntil(max));
      //countPrimitiveTriangles(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
//    println(result.size)
    println("Total Time: " + deltaT + " ms")
  }
}
/*
Triangles for         2:         0
Triangles for         3:         0
Triangles for         4:         0
Triangles for         5:         0
Triangles for         6:         0
Triangles for         7:         1
Triangles for         8:         1
Triangles for         9:         2
Triangles for        10:         2
Triangles for        11:         5
Triangles for        12:         3
Triangles for        13:         8
Triangles for        14:         6
Triangles for        15:         9
Triangles for        16:         9
Triangles for        17:        16
Triangles for        18:        10
Triangles for        19:        21
Triangles for        20:        15
Triangles for        21:        22
Triangles for        22:        20
Triangles for        23:        33
Triangles for        24:        21
Triangles for        25:        38
Triangles for        26:        30
Triangles for        27:        41
Triangles for        28:        35
Triangles for        29:        56
Triangles for        30:        34
Triangles for        31:        65
Triangles for        32:        49
Triangles for        33:        64
Triangles for        34:        56
Triangles for        35:        79
Triangles for        36:        55
Triangles for        37:        96
Triangles for        38:        72
Triangles for        39:        93
Triangles for        40:        77
Triangles for        41:       120
Triangles for        42:        76
Triangles for        43:       133
Triangles for        44:        99
Triangles for        45:       122
Triangles for        46:       110
Triangles for        47:       161
Triangles for        48:       105
Triangles for        49:       172
Triangles for        50:       126
Triangles for        51:       167
Triangles for        52:       143
Triangles for        53:       208
Triangles for        54:       136
Triangles for        55:       213
Triangles for        56:       165
Triangles for        57:       212
Triangles for        58:       182
Triangles for        59:       261
Triangles for        60:       163
Triangles for        61:       280
Triangles for        62:       210
Triangles for        63:       257
Triangles for        64:       225
Triangles for        65:       304
Triangles for        66:       208
Triangles for        67:       341
Triangles for        68:       255
Triangles for        69:       318
Triangles for        70:       254
Triangles for        71:       385
Triangles for        72:       253
Triangles for        73:       408
Triangles for        74:       306
Triangles for        75:       363
Triangles for        76:       323
Triangles for        77:       442
Triangles for        78:       298
Triangles for        79:       481
Triangles for        80:       345
Triangles for        81:       446
Triangles for        82:       380
Triangles for        83:       533
Triangles for        84:       343
Triangles for        85:       534
Triangles for        86:       420
Triangles for        87:       517
Triangles for        88:       437
Triangles for        89:       616
Triangles for        90:       388
Triangles for        91:       627
Triangles for        92:       483
Triangles for        93:       594
Triangles for        94:       506
Triangles for        95:       673
Triangles for        96:       465
Triangles for        97:       736
Triangles for        98:       540
Triangles for        99:       671
Triangles for       100:       551
==============================
20934
Total Time: 881 ms
*/