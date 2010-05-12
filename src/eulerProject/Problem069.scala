package eulerProject

import scala.collection.immutable._

/**
 * Problem 69: Find the value of n <= 1,000,000 for which n/phi(n) is a 
 * maximum.<br/>
 * 07 May 2004<br/>
 * <br/>
 * Euler's Totient function, phi(n) [sometimes called the phi function], is 
 * used to determine the number of numbers less than n which are relatively 
 * prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine 
 * and relatively prime to nine, phi(9)=6.<br/>
 * <br/>
 * <table border="1">
 * 	<tr>
 * 		<th>n</th>
 * 		<th>Relatively Prime </th>
 * 		<th>phi(n)</th>
 * 		<th>n/phi(n)</th>
 * 	</tr>
 * 	<tr>
 * 		<td>2</td>
 * 		<td>1</td>
 * 		<td>1</td>
 * 		<td>2</td>
 * 	</tr>
 * 	<tr>
 * 		<td>3</td>
 * 		<td>1,2</td>
 * 		<td>2</td>
 * 		<td>1.5</td>
 * 	</tr>
 *  	<tr>
 * 		<td>4</td>
 * 		<td>1,3</td>
 * 		<td>2</td>
 * 		<td>2</td>
 * 	</tr>
 * 	<tr>
 * 		<td>5</td>
 * 		<td>1,2,3,4</td>
 * 		<td>4</td>
 * 		<td>1.25</td>
 * 	</tr>
 * 	<tr>
 * 		<td>6</td>
 * 		<td>1,5</td>
 * 		<td>2</td>
 * 		<td>3</td>
 * 	</tr>
 * 	<tr>
 * 		<td>7</td>
 * 		<td>1,2,3,4,5,6</td>
 * 		<td>6</td>
 * 		<td>1.1666...</td>
 * 	</tr>
 * 	<tr>
 * 		<td>8</td>
 * 		<td>1,3,5,7</td>
 * 		<td>4</td>
 * 		<td>2</td>
 * 	</tr>
 * 	<tr>
 * 		<td>9</td>
 * 		<td>1,2,4,5,7,8</td>
 * 		<td>6</td>
 * 		<td>1.5</td>
 * 	</tr>
 * 	<tr>
 * 		<td>10</td>
 * 		<td>1,3,7,9</td>
 * 		<td>4</td>
 * 		<td>2.5</td>
 * 	</tr>
 * </table>
 * <br/>
 * It can be seen that n=6 produces a maximum n/phi(n) for n <= 10.<br/>
 * <br/>
 * <b>Find the value of n <= 1,000,000 for which n/phi(n) is a maximum.</b><br/>
 * 
 */
object Problem069 {
  
  import Utils._
  
  
  def getDivisors(n: Int): SortedSet[Int] = {
    
    val limit = n / 2
    
    def calculate(x: Int, divisors: SortedSet[Int]): SortedSet[Int] = {
      if(divisors(x) || x > limit) divisors
      else /%(n, x) match {
        case (q, 0) => calculate(x + 1, divisors + q + (n / q))
        case _ => calculate(x + 1, divisors)
      }
    }
    
    calculate(2, TreeSet(n))
  }
  
   
  
  def getValue(max: Int): Int = {
    
    def getIntersection(divisorsN: Set[Int], divisors: Set[Int]): Set[Int] = {
      val intersection = divisors ** divisorsN
//      log("\t" + divisors + " ** " + divisorsN + " = " + intersection)
      intersection
    }
    
    def evaluate(n: Int, priorDivisors: List[Set[Int]], greatestN: Int, greatestValue: Double): Int = {
      if(n <= max) {
        val divisorsN = getDivisors(n)
        val phi = priorDivisors.filter(divs => getIntersection(divisorsN, divs).size == 0).size + 1
        val value = n.toDouble / phi
        if(n % 1000 == 0) log("\tphi(%,9d) = %,5d".format(n, phi))
        if(value > greatestValue) {
          log("Phi(" + n + ") = " + phi)
          evaluate(n + 1, divisorsN :: priorDivisors, n, value)
        } else evaluate(n + 1, divisorsN :: priorDivisors, greatestN, greatestValue)
      } else {
        greatestN
      }
    }
    
    evaluate(3, List(Set(2)), 2, 1)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 10000
    
    val t0 = System.currentTimeMillis
    val result = getValue(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
