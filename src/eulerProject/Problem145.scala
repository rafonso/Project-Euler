package eulerProject

/**
 * Problem 148: Exploring Pascal's triangle.<br>
 * 07 April 2007<br>
 * <br>
 * We can easily verify that none of the entries in the first seven rows of 
 * Pascal's triangle are divisible by 7:
 * <pre>
 * 1
 * 1  1
 * 1  2  1
 * 1  3  3  1
 * 1  4  6  4  1
 * 1  5 10 10  5  1
 * 1  6 15 20 15  6  1
 * </pre>
 * However, if we check the first one hundred rows, we will find that only 
 * 2361 of the 5050 entries are not divisible by 7.<br>
 * <br>
 * <b>Find the number of entries which are not divisible by 7 in the first 
 * one billion (10^(9)) rows of Pascal's triangle.</b><br>
 * 
 */
object Problem145 {
  
  def getNextLine(line: Int, previousLine: List[BigInt]): List[BigInt] = {
    val quantity = if(line % 2 == 0) (line / 2) else (line / 2) + 1
    /*
    
    def getEntry(i : Int): BigInt = 
      if(i == 1) 1 
      else if(i == quantity) 1
      else previousLine(i - 1) + previousLine(i)
    
    (1 to quantity).map(getEntry(_)).toList
    */
    
    def generate(i: Int, myLine: List[BigInt]): List[BigInt] = {
      if(i == line) 1 :: myLine 
      else generate(i + 1, (previousLine(i) + previousLine(i - 1)) :: myLine)
    }
    
    generate(1, List(1))
  }
  
  def getTriangule(line: Int, actualLine: List[BigInt], max: Int): Unit = {
    println(line + " => " + actualLine.size)
    if(line <= max) {
      getTriangule(line + 1, getNextLine(line, actualLine), max)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    getTriangule(1, List(1), 10000)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println("Time = " + deltaT + " ms")
  }
}
/*
List(1)
List(1,  1)
List(1,  2,  1)
List(1,  3,  3,   1)
List(1,  4,  6,   4,   1)
List(1,  5, 10,  10,   5,   1)
List(1,  6, 15,  20,  15,   6,   1)
List(1,  7, 21,  35,  35,  21,   7,   1)
List(1,  8, 28,  56,  70,  56,  28,   8,  1)
List(1,  9, 36,  84, 126, 126,  84,  36,  9,  1)
List(1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1)

*/