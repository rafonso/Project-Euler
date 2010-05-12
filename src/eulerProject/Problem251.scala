package eulerProject

/**
 * Problem 251: Cardano Triplets.<br>i
 * 20 June 2009<br>
 * <br>
 * A triplet of positive integers (a,b,c) is called a Cardano Triplet if it 
 * satisfies the condition:<br>
 * cubeSquare(a + b * root(c)) + cubeSquare(a - b * root(c)) = 1<br>
 * For example, (2,1,5) is a Cardano Triplet.<br>
 * <br>
 * There exist 149 Cardano Triplets for which a+b+c <= 1000.<br>
 * <br>
 * <b>Find how many Cardano Triplets exist such that a+b+c <= 110,000,000.</b><br>
 * <br>
 * Note: This problem has been changed recently, please check that you are 
 * using the right parameters.<br>
 * <br>
 */
object Problem251 {
  
  import java.lang.Math._
  
  def calculateTerm(a: Int, b: Int, c: Int) = cbrt(a + b * sqrt(c)) + cbrt(a - b * sqrt(c))
  
  val tolerance = pow(10, -12)
  val min = 1.0 - tolerance
  val max = 1.0 + tolerance
  
  def isCardano(a: Int, b: Int, c: Int): Boolean = {
    println(a + ", " + b + ", " + c)
    val term = calculateTerm(a, b, c)
    (term >= min) && (term <= max) 
  }
  
  def sortTriplets(tripletA: (Int, Int, Int), tripletB: (Int, Int, Int)): Boolean = {
    if(tripletA._1 != tripletB._1) tripletA._1 < tripletB._1
    else if(tripletA._2 != tripletB._2) tripletA._2 < tripletB._2
    else tripletA._3 < tripletB._3
  }
  
  def getTermsFor(max: Int): List[(Int, Int, Int)] = {
    for{
        a <- List.range(1, max); 
        b <- List.range(1, max); 
        if (max > a + b);
        c = max - a - b
        if(isCardano(a, b, c))
    } 
    yield (a, b, c)
  }
  
  def getTermsUntil(max: Int): Int = {
    
    def eval(i: Int, sum: Int): Int =
      if(i > max) sum
      else { 
        val tripletsForI = getTermsFor(i)
        if(!tripletsForI.isEmpty) printf("%,7d => %s%n", i, tripletsForI)
        eval(i + 1, sum + tripletsForI.size)
      }
    
    eval(1, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val terms = getTermsUntil(1000)
    val deltaT = System.currentTimeMillis - t0
    
//    terms.foreach(t => println(t + " = " + calculateTerm(t._1, t._2, t._3)))
    println("========================")
    println("SIZE: " + terms)
    println("TIME: " + deltaT + " ms")
  }
  // 157490 ms
}

