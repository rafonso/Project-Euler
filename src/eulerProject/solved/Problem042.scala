package eulerProject.solved

/**
 * Problem 42: How many triangle words does the list of common English words 
 * contain?<br/>
 * 25 April 2003<br/>
 * <br/>
 * The n^(th) term of the sequence of triangle numbers is given by, 
 * t(n) = n(n+1)/2; so the first ten triangle numbers are:<br/>
 * <br/>
 * 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...<br/>
 * <br/>
 * By converting each letter in a word to a number corresponding to its 
 * alphabetical position and adding these values we form a word value. For 
 * example, the word value for SKY is 19 + 11 + 25 = 55 = t(10). If the word 
 * value is a triangle number then we shall call the word a triangle word.<br/>
 * <br/>
 * Using words.txt (right click and 'Save Link/Target As...'), a 16K text file 
 * containing nearly two-thousand common English words, how many are triangle 
 * words?<br/>
 * <br/>
 */
object Problem042 {
  
      
  import scala.collection.immutable.TreeHashMap
  
  val charValues = TreeHashMap.fromIterable(('A' to 'Z').map(c => (c, c - 'A' + 1)))
  
  def isTriangular(t: Long): Boolean= {
    val sqrtDelta = Utils.getSqrt(8 * t + 1)
    if(sqrtDelta.isRight) ((sqrtDelta.right.get - 1) % 2 == 0)
    else false
  }
  
  def stringValue(word: String): Long = word.foldLeft(0L)(_ + charValues(_))
  
  def isTriangularWord(word: String) = isTriangular(stringValue(word))
  
  def getWords: Array[String] = {
    import scala.io._
    
    val sourceWords = Source.fromFile("words.txt")
    val line = sourceWords.getLines.next
    line.split("[,\"]").filter(_.size > 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val words = getWords
    val result = words.filter(isTriangularWord(_))
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================================================")
    println(result.toString)
    println(result.size)
    println("Time = " + deltaT + " ms")
  }
}
