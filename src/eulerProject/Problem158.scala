package eulerProject

/**
 * Problem 158: Exploring strings for which only one character comes 
 * lexicographically after its neighbour to the left.<br>
 * 15 June 2007<br>
 * <br>
 * Taking three different letters from the 26 letters of the alphabet, 
 * character strings of length three can be formed.<br>
 * Examples are 'abc', 'hat' and 'zyx'.<br>
 * When we study these three examples we see that for 'abc' two characters 
 * come lexicographically after its neighbour to the left.<br>
 * For 'hat' there is exactly one character that comes lexicographically after 
 * its neighbour to the left. For 'zyx' there are zero characters that come 
 * lexicographically after its neighbour to the left.<br>
 * In all there are 10400 strings of length 3 for which exactly one character 
 * comes lexicographically after its neighbour to the left.<br>
 * <br>
 * We now consider strings of n <= 26 different characters from the alphabet.<br>
 * For every n, p(n) is the number of strings of length n for which exactly 
 * one character comes lexicographically after its neighbour to the left.<br>
 * <br>
 * <b>What is the maximum value of p(n)?</b><br>
 * 
 */
object Problem158 {
  
  case class PairCombinations(char1: Char, char2: Char, charsBefore: List[Char], charsAfter: List[Char])
  
  val alphabet =  ('a' to 'z').toList
  val pairs = for{
    ch1 <- alphabet; 
    ch2 <- ((ch1 + 1).toChar to 'z')
  } yield (ch1, ch2)
  
  val combinations = for{
    ch1 <- alphabet; 
    ch2 <- ((ch1 + 1).toChar to 'z');
    charsBefore = alphabet.filter(_ != ch2).filter(_ > ch1).reverse
    charsAfter  = alphabet.reverse -- (ch1 :: ch2 :: charsBefore)
  } yield PairCombinations(ch1, ch2, charsBefore, charsAfter)
  
  def getCombinations(chars: List[Char], size: Int): List[List[Char]] = {
    
    def eval(currentChars: List[Char], currentCombinations: List[List[Char]] ): List[List[Char]] = currentChars match {
      case List(ch) => currentCombinations.filter(_.size == size)
      case first :: others =>
        val (comboSize, comboSmall) = currentCombinations.span(_.size == size)
        val newCombos = comboSmall.flatMap(combo => currentChars.filter(_ < combo.last).map(combo ::: List(_)))
        val pairs = others.map(List(first, _))
        /*
        println(currentChars.first)
        println("\tWith " + size + " => " + comboSize)
        println("\tMinus than " + size + " => " + comboSmall)
        println("\tNew Combos => " + newCombos)
        println("\tPairs => " + pairs + '\n')
        */
        eval(others, comboSize ::: newCombos ::: pairs)
    }
    
    size match {
      case 0 => Nil
      case 1 => chars.map(List(_))
      case s if(s == chars.size) => List(chars)
      case s if(s > chars.size) => Nil
        //error("Request size (" + s + ") greater than chars size (" + chars.size + ")")
      case _ => eval(chars, Nil)
    }
  }
  
  def p(n: Int): Long = {
    
    def showCombinations(pair: String, showBefore: Boolean, showAfter: Boolean, pos: Int, combos: List[String]) = {
      print("- " + pair + " " + pos + " ")
      if(showBefore) print("B") else print(" ")
      if(showAfter)  print("A") else print(" ")
      print(": ")
      println(combos)
    }
    
    def evalPosition(position: Int, pairCombination: PairCombinations): Long = {
      val showBefore = ( position      > 0)
      val showAfter  = ((position + 2) < n)
      
      if(showBefore && pairCombination.charsBefore.isEmpty) {
        0
      } else if(showAfter && pairCombination.charsAfter.isEmpty) {
        0
      } else {
        val combinationsBefore = if(showBefore) getCombinations(pairCombination.charsBefore,      position     ) else List(Nil)
        val combinationsAfter  = if(showAfter)  getCombinations(pairCombination.charsAfter,  n - (position + 2)) else List(Nil)
        val pair = "" + pairCombination.char1 + pairCombination.char2
        val combos = for{chsBef <- combinationsBefore; chsAft <- combinationsAfter} yield chsBef.mkString + pair + chsAft.mkString
        showCombinations(pair, showBefore, showAfter, position, combos)
        combos.size
      }
    }
    
    def evalPairCombination(pairCombination: PairCombinations): Long = {
      println("* " + pairCombination)
      (0 to (n - 2)).foldLeft(0L)(_ + evalPosition(_, pairCombination))
    }
    
    println("---------------------------------")
    println(n)
    combinations.foldLeft(0L)(_ + evalPairCombination(_))
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = p(3)
    val deltaT = System.currentTimeMillis - t0
    
    println("=========================================================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}