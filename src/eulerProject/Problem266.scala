package eulerProject

import scala.collection.immutable._

/**
 * Problem 266: Pseudo Square Root<br/>
 * 28 November 2009<br/>
 * <br/>
 * The divisors of 12 are: 1,2,3,4,6 and 12.<br/>
 * The largest divisor of 12 that does not exceed the square root of 12 is 3.<br/>
 * We shall call the largest divisor of an integer n that does not exceed the 
 * square root of n the pseudo square root (PSR) of n.<br/>
 * It can be seen that PSR(3102)=47.<br/>
 * <br/>
 * Let p be the product of the primes below 190.<br/>
 * <b>Find PSR(p) mod 10<sup>16</sup>.</b><br/>
 * <br/>
 */
object Problem266 {
  
  def getSqrt(n: BigInt): BigInt = {
    
    def evaluate(rootAnt: BigInt, root: BigInt): BigInt = {
      if(rootAnt <= root) root
      else evaluate(root, (root + n / root) / 2)
    }
    
    evaluate(n * 2, n)
  }
  
  val ONE = BigInt(1)
  
  case class ProductFactors(product: BigInt, factors: SortedSet[Long]) {
    
    def getNew(newFactor: Long) = ProductFactors(product * newFactor, factors + newFactor)
    
  }
  
  def getGreatestProductSmallerThan(max: BigInt, numbers: List[Long]): ProductFactors = {
    
    val empty = ProductFactors(ONE, TreeSet.empty[Long])
    
    def evaluate(myNumbers: List[Long], prior: ProductFactors, priorGreatest: ProductFactors): ProductFactors = myNumbers match {
      case x :: xs => {
        val current = prior.getNew(x)
//        println(log + "starting  -> greatest = " + priorGreatest + ", prior = " + prior + ", current = " + current + ", xs = " + xs)
        if(current.product > max) {
          priorGreatest
//          evaluate(xs, prior, priorGreatest)
        } else if(current.product == max) {
          current
        } else { // (current.product < max) 
          val greatestForX = if(current.product > priorGreatest.product) {
            val now = new java.util.Date
            val log = "[%tT.%tL - %3d]: ".format(now, now, x)
            println(log + current)
            current
          } else {
            priorGreatest
          }
          
//          println(log + "greatest = " + greatestForX + ", xs = " + xs)
          val greatest1 = evaluate(xs, current, greatestForX)
//          println(log + "greatest1 -> greatest = " + greatest1 + ", current = " + current + ", xs = " + xs)
          xs match {
            case y :: ys => {
              val greatest2 = evaluate(ys, current, greatest1)
//              println(log + "greatest2 -> greatest = " + greatest2 + ", ys = " + ys)
              evaluate(ys, empty, greatest2)
            }
            case Nil => greatest1
          }
        }
      }
      case Nil => priorGreatest
    }
    
    evaluate(numbers, empty, empty)
  }
  
  val base16 = BigInt("10000000000000000")
  
  def main(args : Array[String]) : Unit = {
    
    val divisors: List[Long] = Utils.getPrimesUntil(190)
      //List(2, 3, 11, 17, 57, 101, 991, 23)
    val base = divisors.foldLeft(ONE)(_ * _)
    val sqrt = getSqrt(base)
    
    val t0 = System.currentTimeMillis
    val result = getGreatestProductSmallerThan(sqrt, divisors)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println("[%tT]".format(new java.util.Date))
    println("divisors = " + divisors)
    println("product  = " + base)
    println("Sqr root = " + sqrt)
    println("PSR      = " + result.product)
    println("Last 16  = " + result.product % base16)
    println("Time = " + deltaT + " ms")
  }
}
/*
[19:11:04.720 -   2]: ProductFactors(2,Set(2))
[19:11:04.757 -   3]: ProductFactors(6,Set(2, 3))
[19:11:04.757 -   5]: ProductFactors(30,Set(2, 3, 5))
[19:11:04.759 -   7]: ProductFactors(210,Set(2, 3, 5, 7))
[19:11:04.759 -  11]: ProductFactors(2310,Set(2, 3, 5, 7, 11))
[19:11:04.759 -  13]: ProductFactors(30030,Set(2, 3, 5, 7, 11, 13))
[19:11:04.761 -  17]: ProductFactors(510510,Set(2, 3, 5, 7, 11, 13, 17))
[19:11:04.761 -  19]: ProductFactors(9699690,Set(2, 3, 5, 7, 11, 13, 17, 19))
[19:11:04.763 -  23]: ProductFactors(223092870,Set(2, 3, 5, 7, 11, 13, 17, 19, 23))
[19:11:04.763 -  29]: ProductFactors(6469693230,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
[19:11:04.765 -  31]: ProductFactors(200560490130,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31))
[19:11:04.765 -  37]: ProductFactors(7420738134810,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37))
[19:11:04.767 -  41]: ProductFactors(304250263527210,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41))
[19:11:04.767 -  43]: ProductFactors(13082761331670030,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43))
[19:11:04.767 -  47]: ProductFactors(614889782588491410,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47))
[19:11:04.769 -  53]: ProductFactors(32589158477190044730,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53))
[19:11:04.773 -  59]: ProductFactors(1922760350154212639070,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59))
[19:11:04.773 -  61]: ProductFactors(117288381359406970983270,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61))
[19:11:04.775 -  67]: ProductFactors(7858321551080267055879090,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67))
[19:11:04.775 -  71]: ProductFactors(557940830126698960967415390,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71))
[19:11:04.777 -  73]: ProductFactors(40729680599249024150621323470,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73))
[19:11:04.777 -  79]: ProductFactors(3217644767340672907899084554130,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79))
[19:11:04.779 -  83]: ProductFactors(267064515689275851355624017992790,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83))
[19:11:04.779 -  89]: ProductFactors(23768741896345550770650537601358310,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89))
[19:11:04.779 -  97]: ProductFactors(2305567963945518424753102147331756070,Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97))
[19:11:40.248 - 179]: ProductFactors(2313458632843456829550297809464289549,Set(61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 109, 127, 131, 139, 151, 163, 167, 179))
[19:11:40.260 - 179]: ProductFactors(2314836593789816622765826011149597959,Set(61, 67, 71, 73, 79, 83, 89, 97, 101, 107, 109, 127, 131, 139, 151, 157, 167, 179))
[19:11:40.260 - 173]: ProductFactors(2322744088114401983609348741714675047,Set(61, 67, 71, 73, 79, 83, 89, 97, 101, 107, 109, 127, 131, 139, 151, 163, 167, 173))
[19:11:40.601 - 163]: ProductFactors(2323104151359807133661792872698193417,Set(61, 67, 71, 73, 79, 89, 97, 101, 107, 109, 113, 127, 131, 137, 139, 149, 157, 163))
[19:12:20.007 - 181]: ProductFactors(2323121327894429310092911138281353957,Set(59, 61, 67, 73, 79, 83, 89, 101, 103, 107, 109, 127, 137, 139, 151, 163, 173, 181))
[19:13:37.087 - 179]: ProductFactors(2323129666395596482755494204446462339,Set(53, 59, 61, 71, 73, 83, 97, 103, 107, 109, 113, 131, 139, 149, 157, 163, 173, 179))
[19:13:37.921 - 179]: ProductFactors(2323177201048641473150949803608352729,Set(53, 59, 61, 71, 79, 83, 97, 101, 103, 109, 113, 127, 137, 149, 157, 167, 173, 179))
[19:30:14.727 - 139]: ProductFactors(2323190937470955870661077670237018963,Set(41, 43, 47, 53, 59, 67, 71, 73, 79, 83, 97, 101, 103, 109, 113, 127, 131, 137, 139))
[19:54:47.890 - 151]: ProductFactors(2323192856402841885831172448273044651,Set(37, 41, 43, 47, 53, 61, 71, 73, 83, 97, 101, 103, 107, 109, 113, 131, 139, 149, 151))
[20:46:17.861 - 163]: ProductFactors(2323204444743582827395636877833405759,Set(31, 37, 43, 47, 59, 61, 71, 73, 83, 89, 97, 101, 107, 113, 131, 137, 139, 151, 163))
[20:54:35.849 - 157]: ProductFactors(2323205517148212549694791794063279779,Set(31, 41, 43, 47, 59, 61, 71, 73, 83, 89, 101, 103, 107, 109, 127, 131, 139, 149, 157))
[22:27:38.505 - 163]: ProductFactors(2323212644650512839798569118471914999,Set(29, 31, 41, 47, 53, 61, 71, 79, 83, 97, 101, 107, 109, 127, 131, 137, 139, 151, 163))

 */