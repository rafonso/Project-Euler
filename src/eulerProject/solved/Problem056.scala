package eulerProject.solved

/**
 * Problem 56: <br>
 * 07 November 2003<br>
 * <br>
 * A googol (10^(100)) is a massive number: one followed by one-hundred zeros; 
 * 100^(100) is almost unimaginably large: one followed by two-hundred zeros. 
 * Despite their size, the sum of the digits in each number is only 1.<br>
 * <br>
 * <b>Considering natural numbers of the form, a^(b), where a, b < 100, 
 * what is the maximum digital sum?</b><br>
 * 
 */
object Problem056 {
  
  def getGratestDigitsSum: BigInt = {
    val numbers = (1 until 100).map(BigInt(_)).toList
    
    def sumDigits(n: BigInt, sum: BigInt): BigInt = {
      val (div, rem) = n /% 10
      if(n == 0) sum
      else sumDigits(div, sum + rem)
    }
    
    def evaluateBaseExpoents(base: BigInt, expoents: List[BigInt], gretatestValue: BigInt): BigInt = expoents match {
      case Nil => gretatestValue
      case exp :: others => {
        val pow = base.pow(exp.intValue)
        val sum = sumDigits(pow, 0)
        println("sum(%2d, %2d) = %d".format(base.intValue, exp.intValue, sum.intValue))
        if(sum > gretatestValue) evaluateBaseExpoents(base, others, sum)
        else evaluateBaseExpoents(base, others, gretatestValue)
      }
    }
    
    def evaluateBase(bases: List[BigInt], gretatestValue: BigInt): BigInt = bases match {
      case Nil => gretatestValue
      case b :: others => {
        val baseSum = evaluateBaseExpoents(b, numbers, 0)
        if(baseSum > gretatestValue) evaluateBase(others, baseSum)
        else evaluateBase(others, gretatestValue)
      }
    }
    
    evaluateBase(numbers, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = getGratestDigitsSum
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
