package eulerProject.solved

/**
 Problem 206: Concealed Square.<br>
06 September 2008<br>
<br>
Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
where each “_” is a single digit.<br>
<br>
  Result: <i>1389019170</i>
 * EULER: SOLVED
 */
object Problem206 {
  val pattern = java.util.regex.Pattern.compile("1.2.3.4.5.6.7.8.9.0")
  
  def matches(n: Int): Boolean = {
    val square = n.toLong * n.toLong
    val last3 = square % 1000
    last3 match {
      case 900 | 910 | 920 | 930 | 940 | 950 | 960 | 970 | 980 | 990 => {
//        println(n)
        pattern.matcher(square.toString).matches
      }
      case _ => false
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val start = Math.sqrt(1020304050607080900L).toInt // 1010101010
    val end   = Math.sqrt(1929394959697989990L).toInt // 1389026623
    println("FROM " + start + " until " + end)
    val t0 = System.currentTimeMillis
    val result = (start to end by 10).find(matches(_))
    val deltaT = System.currentTimeMillis - t0
    println("Result: " + result)
    println("Time = " + deltaT + " ms")
  }
}
