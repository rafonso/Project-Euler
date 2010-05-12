package eulerProject

/**
 * Problem 152: Writing 1/2 as a sum of inverse squares.<br/>
 * 27 April 2007<br/>
 * <br/>
 * There are several ways to write the number 1/2 as a sum of inverse squares 
 * using distinct integers.<br/>
 * <br/>
 * For instance, the numbers {2,3,4,5,7,12,15,20,28,35} can be used:<br/>
 * <br/>
 * <img src="http://projecteuler.net/project/images/p_152_sum.gif" alt="" border="0"/><br/>
 * <br/>
 * In fact, only using integers between 2 and 45 inclusive, there are exactly 
 * three ways to do it, the remaining two being: {2,3,4,6,7,9,10,20,28,35,36,45} 
 * and {2,3,4,6,7,9,12,15,28,30,35,36,45}.<br/>
 * <br/>
 * <b>How many ways are there to write the number 1/2 as a sum of inverse 
 * squares using distinct integers between 2 and 80 inclusive?</b><br/>
 * <br/>
 */
object Problem152 {
  
  val TWO  = BigInt(2)
  val ONE  = BigInt(1)
  val ZERO = BigInt(0)
  
  type Combinations = List[List[Int]]
  
  def getQuantity(numbers: List[Int]): Int = {
    
    def show(comb: List[Int]): List[Int] = {
      println(comb)
      comb
    }
    
    def evaluateCombination(combination: List[Int]): (BigInt, BigInt) = {
      val denominator = combination.foldLeft(ONE) ((prod, i) => prod * i * i)
      val numerator   = combination.foldLeft(ZERO)((sum, i) =>  sum + denominator / (i * i))
      denominator /% numerator
    }
    
    def evaluateCombinationsForX(combinations: Combinations): (Combinations, Combinations, Combinations) = {
      
      def evaluate(combRemained: Combinations, combLesser2: Combinations, combEqual2: Combinations, combGreater2: Combinations):(Combinations, Combinations, Combinations) = combRemained match {
        case comb :: others => {
          val (quocient, remainder) = evaluateCombination(comb)
          
          if(quocient > TWO) evaluate(others, comb :: combLesser2, combEqual2, combGreater2)
          else if(quocient == 2 && remainder == ZERO) evaluate(others, combLesser2, comb :: combLesser2, combGreater2)
          else evaluate(others, combLesser2, combEqual2, comb :: combLesser2)
        }
        case Nil => (combLesser2, combEqual2, combGreater2)
      }
      
      evaluate(combinations, Nil, Nil, Nil)
    }
    
    def generate(list: List[Int], previousCombinations: Combinations, previousQuantity: Int): Int = list match {
      case Nil => previousQuantity
      case x :: xs => {
        println(x)
        
        val currentCombinations = List(x) :: previousCombinations.map(combination => x :: combination)
        val (lesser2, equals2, greater2) = evaluateCombinationsForX(currentCombinations)
        
        println('\t' + "Lesser  than 2: " + lesser2.size)
        println('\t' + "Equals  than 2: " + equals2)
        println('\t' + "Greater than 2: " + greater2.size)
        
        generate(xs, previousCombinations ::: lesser2, previousQuantity + equals2.size)
      }
    }
    
    generate(numbers, Nil, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val combinations = (2 to 35).toList
    // 2,3,4,5,7,12,15,20,28,35
    
    val t0 = System.currentTimeMillis
    val quantity = getQuantity(combinations)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(quantity)
    println("Time = " + deltaT + " ms")
  }
}
