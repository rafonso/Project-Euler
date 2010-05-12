package eulerProject.solved

/**
 * EULER: SOLVED
 */
object Problem120c {
  
  def maxRemainder(a: Int): Int = {
    val aSquare = a * a
    val aMinus1 = a - 1
    val aPlus1  = a + 1
    
    val max = if(a % 2 != 0){
      a * 2
    } else if(a % 4 == 0) {
      a / 2
    } else {
      a
    }
    
    def evaluate(index: Int, aMinus1Acc: BigInt, aPlus1Acc: BigInt, result: Int): Int = {
      if(index > max) result
      else {
        val value = ((aMinus1Acc + aPlus1Acc) % aSquare).intValue
        val nextResult = if(value > result) value else result
        evaluate(index + 1, aMinus1Acc * aMinus1, aPlus1Acc * aPlus1, nextResult)
      }
    }
    
    println(a)
    evaluate(1, aMinus1, aPlus1, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val maxRemainders = (3 to 1000).map(maxRemainder(_))
    val sum = maxRemainders.foldLeft(0)(_+_)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
//    println(maxRemainders)
    println(sum)
    println("Time = " + deltaT + " ms")
  }
}
// Array(6, 8, 20, 24, 42, 48, 72, 80, 110, 120, 156, 168, 210, 224, 272, 288, 342, 360)
