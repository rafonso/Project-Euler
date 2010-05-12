package eulerProject.solved

class PascalTrianguleIterator extends BufferedIterator[Array[BigInt]] {
  
  val BIG_ONE = BigInt(1)
  
  private var currentLine = 0
  
  private var currentRow: Array[BigInt] = Array()
  
  private def makeNextLine = {
    val nextLine = new Array[BigInt](currentLine)
    nextLine(0) = BIG_ONE
    nextLine(currentLine - 1) = BIG_ONE
    (1 until currentLine - 1).foreach(i => nextLine(i) = currentRow(i - 1) + currentRow(i))
    nextLine
  }
  
  def head: Array[BigInt] = currentRow
  
  def hasNext: Boolean = true
  
  def line = this.currentLine
  
  def next: Array[BigInt] = {
    this.currentLine += 1
    this.currentRow = this.makeNextLine
    
    this.currentRow 
  }
  
}

/**
 * Problem 203: Squarefree Binomial Coefficients<br/>
 * 06 September 2008<br/>
 * <br/>
 * The binomial coefficients <sup>n</sup>C<sub>k</sub> can be arranged in 
 * triangular form, Pascal's triangle, like this:<pre>
 * 1
 * 1  1
 * 1  2  1
 * 1  3  3  1
 * 1  4  6  4  1
 * 1  5 10 10  5  1
 * 1  6 15 20 15  6  1
 * 1  7 21 35 35 21  7  1
 * .........
 * </pre>
 * It can be seen that the first eight rows of Pascal's triangle contain 
 * twelve distinct numbers: 1, 2, 3, 4, 5, 6, 7, 10, 15, 20, 21 and 35.<br/>
 * <br/>
 * A positive integer n is called squarefree if no square of a prime divides n. 
 * Of the twelve distinct numbers in the first eight rows of Pascal's triangle, 
 * all except 4 and 20 are squarefree. The sum of the distinct squarefree 
 * numbers in the first eight rows is 105.<br/>
 * <br/>
 * <b>Find the sum of the distinct squarefree numbers in the first 51 rows 
 * of Pascal's triangle.</b><br/>
 * 
 */
object Problem203 {
  
  println("Getting Primes")
  val primes: List[Long] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997)
    //PrimesGenerator.getPrimes
  
  import scala.collection.Set
  
  def squareFreesFromPascal(max: Int): Set[BigInt] = {
    
    def isSquareFree(n: BigInt) = !primes
      .takeWhile(p => BigInt(p * p) <= n)
      .exists(p => (n % (p * p) == 0))
    
    import scala.collection.immutable.TreeSet
    
    def arrayToTreeSet(array: Array[BigInt]) = array.foldLeft(new TreeSet[BigInt])(_ + _)
    
    def eval(it: PascalTrianguleIterator, results: Set[BigInt]): Set[BigInt] = {
      if(it.line >= max) {
        results
      } else {
        val row = it.next
        val squaresFree = arrayToTreeSet(row).filter(isSquareFree(_))
        eval(it, squaresFree ++ results)
      }
    }
    
    eval(new PascalTrianguleIterator, new TreeSet())
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 51
    
    val t0 = System.currentTimeMillis
    val result = squareFreesFromPascal(max)
    val sum = result.foldLeft(BigInt(0))(_ + _)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println(sum)
    println("Time = " + deltaT + " ms")
  }
}