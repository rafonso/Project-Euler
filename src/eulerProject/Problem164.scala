package eulerProject

/**
 * Problem 164: Numbers for which no three consecutive digits have a sum 
 * greater than a given value.<br/>
 * 20 October 2007<br/>
 * <br/>
 * <b>How many 20 digit numbers n (without any leading zero) exist such that 
 * no three consecutive digits of n have a sum greater than 9?</b><br/>
 * <br/>
 */
object Problem164 {
  
  import Utils._
  
  val digits = (0 to 9).toList
  
  def getNextNumbers(number: Int, groupSize: Int, groupMaxValue: Int): List[Int] = {
    val strNumber = number.toString
    val strDigits = if(strNumber.size > groupSize) strNumber.reverse.take(groupSize).toSeq else strNumber.toSeq
    val sumDigits = strDigits.map(_.toInt - '0').foldLeft(0)(_ + _)
    if(sumDigits > groupMaxValue) Nil
    else {
      val nextNumber = if(strNumber.size > groupSize) (strDigits.reverse.mkString.toInt * 10) else (strDigits.mkString.toInt * 10) 
      digits.filter(_ + sumDigits <= 9).map(nextNumber + _)
    }
  }
  
  def countValidNumbers(sizeNumbers: Int, groupSize: Int, groupMaxValue: Int): Long = {
    
    def calculate(size: Int, currentNumbers: List[Int]): Long = {
      if(size <= sizeNumbers) {
        val nextNumbers = currentNumbers.flatMap(getNextNumbers(_, groupSize, groupMaxValue))
        log("Size %2d: %,20d".format(size, nextNumbers.size))
        calculate(size + 1, nextNumbers)
      } else {
        currentNumbers.size
      }
    }
    
    calculate(1, (1 to 9).toList)
  }
  
  def main(args : Array[String]) : Unit = {
    val sizeNumbers = 20
    val groupSize = 3
    val groupMaxValue = 9
    
    val t0 = System.currentTimeMillis 
    val result = countValidNumbers(sizeNumbers, groupSize, groupMaxValue)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
