package eulerProject.solved

/**
 * Problem 17: How many letters would be needed to write all the numbers in 
 * words from 1 to 1000?<br/>
 * 17 May 2002<br/>
 * <br/>
 * If the numbers 1 to 5 are written out in words: one, two, three, four, 
 * five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.<br/>
 * <br/>
 * <b>If all the numbers from 1 to 1000 (one thousand) inclusive were written 
 * out in words, how many letters would be used?</b><br/>
 * <br/>
 * <font size="-2">NOTE: Do not count spaces or hyphens. For example, 342 
 * (three hundred and forty-two) contains 23 letters and 115 (one hundred and 
 * fifteen) contains 20 letters. The use of "and" when writing out numbers is 
 * in compliance with British usage.</font><br/>
 * 
 */
object Problem017 {
  
  val units = Array("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val teens = Array("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
  val tens = Array("", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  val HUNDRED = "hundred"
  val THOUSAND = "thousand"
  val AND = "and"
  val EMPTY = ""
  val SPC = ""
  
  def numberToString(i: Int): String = {
    
    def convertHundreds(x: Int): String = {
      val myHund = x / 100
      val myTen  = x % 100
      
      val hundredStr = if(myHund > 0) units(myHund) + SPC + HUNDRED else EMPTY 
      val tenStr = myTen match {
        case 0 => EMPTY 
        case n if(n < 10) => units(n)
        case n if((n >= 10) && (n < 20)) => teens(n % 10)
        case _ => tens(myTen / 10) + (if(myTen % 10 > 0) SPC + units(myTen % 10) else EMPTY)
      }
      val middle = if(myHund > 0 && myTen > 0) SPC + AND + SPC else EMPTY 
      
      hundredStr + middle + tenStr
    }
    
    
    if(i > 1000000) {
      error("Number must be lesser than 1,000,000. But it is " + i)
    }
    
    if(i >= 1000) {
      val thousand = i / 1000
      val hundred  = i % 1000
      convertHundreds(thousand) + SPC + THOUSAND + (if(hundred > 0) SPC + AND + SPC + convertHundreds(hundred) else EMPTY)
    } else {
      convertHundreds(i)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val names = (1 to 1000).map(numberToString(_))
    val sizes = names.foldLeft(0)(_ + _.size)
    val deltaT = System.currentTimeMillis - t0
    
    names.foreach(println(_))
    println("==================")
    println(sizes)
    println("Time = " + deltaT + " ms")
    
  }
}
