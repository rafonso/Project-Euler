package eulerProject.solved

/**
 * 
 * Problem 119: Investigating the numbers which are equal to sum of their 
 * digits raised to some power.<br/>
 * 07 April 2006<br/>
 * <br/>
 * The number 512 is interesting because it is equal to the sum of its digits 
 * raised to some power: 5 + 1 + 2 = 8, and 8<sup>3</sup> = 512. Another 
 * example of a number with this property is 614656 = 28<sup>4</sup>.<br/>
 * <br/>
 * We shall define a<sub>n</sub> to be the nth term of this sequence and insist 
 * that a number must contain at least two digits to have a sum.<br/>
 * <br/>
 * You are given that a<sub>2</sub> = 512 and a<sub>10</sub> = 614656.<br/>
 * <br/>
 * <b>Find a<sub>30</sub>.</b><br/>
 * <br/>
 * 
 */
object Problem119 {
  
  val TEN = BigInt(10)
  
  case class Power(base: Int, expoent: Int, result: BigInt) extends Ordered[Power] {
    
    def compare(other: Power): Int = this.result.compare(other.result)
    
  }
  
  def sumDigitsNumber(n: BigInt): Int = {
	
	def sumDigits(number: BigInt, sum: Int): Int = {
		if(number == 0) sum
		else sumDigits((number / TEN), sum + (number % TEN).intValue)
	}

	sumDigits(n, 0)
  }
  
  def findA(max: Long): List[Power] = {
    val sqrtMax = Math.sqrt(max).toLong
    
    def generatePowers(base: Int, currentExpoent: Int, currentResult: BigInt, powers: List[Power]): List[Power] = {
      if(currentResult > max) powers
      else if(sumDigitsNumber(currentResult) == base) {
          val p = Power(base, currentExpoent, currentResult) 
          println(p)
          generatePowers(base, currentExpoent + 1, currentResult * base, p :: powers)
      } else {
        generatePowers(base, currentExpoent + 1, currentResult * base, powers)
      }
    }

    (2 to sqrtMax.toInt)
      .flatMap(base => generatePowers(base, 1, base, Nil))
      .filter(power => !(power.base < 10 && power.expoent == 1))
      .toList
      .sort((power1, power2) => power1.compare(power2) < 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val size = 30
    val max = 1000000000000000L 
    
    val t0 = System.currentTimeMillis
    val result = findA(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println(result.size)
    if(result.size >= size) {
      println("a(" + size + ") = " + result(size - 1))
    }
    println("Time = " + deltaT + " ms")
  }
}
