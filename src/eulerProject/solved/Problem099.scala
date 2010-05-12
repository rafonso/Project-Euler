package eulerProject.solved


/**
 * Problem 99: Which base/exponent pair in the file has the greatest numerical 
 * value?<br/>
 * 01 July 2005<br/>
 * <br/>
 * Comparing two numbers written in index form like 2<sup>11</sup> and 
 * 3<sup>7</sup> is not difficult, as any calculator would confirm that 
 * 2<sup>11</sup> = 2048 < 3<sup>7</sup> = 2187.<br/>
 * <br/>
 * However, confirming that 632382<sup>518061</sup> > 519432<sup>525806</sup> 
 * would be much more difficult, as both numbers contain over three million 
 * digits.<br/>
 * <br/>
 * <b>Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K 
 * text file containing one thousand lines with a base/exponent pair on each 
 * line, determine which line number has the greatest numerical value.</b><br/>
 * <br/>
 * NOTE: The first two lines in the file represent the numbers in the example given above.<br/>
 * <br/>
 */
object Problem099 {
  
  case class Power(line: Int, base: Int, expoent: Int) {
    
    lazy val order: Double = Math.log(base) * expoent
    
    lazy val value = BigInt(base).pow(expoent)
    
  }
  
  def getPowers: List[Power] = {
    
    def generate(itSource: Iterator[String], line: Int, powers: List[Power]): List[Power] = {
      if(itSource.hasNext) {
        val strLines = itSource.next.trim.split(",")
        generate(itSource, line + 1, Power(line, strLines(0).toInt, strLines(1).toInt) :: powers)
      } else {
        powers.reverse
      }
    }
    
    generate(scala.io.Source.fromFile("base_exp.txt").getLines, 1, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val powers = getPowers.sort(_.order < _.order).takeRight(3)
    powers.foreach(p => println(p + ": " + p.order))
  }
}
