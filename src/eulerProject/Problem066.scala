package eulerProject

/**
 * Problem 66: Investigate the Diophantine equation x^2 - Dy^2 = 1.<br>
 * 26 March 2004<br>
 * <br>
 * Consider quadratic Diophantine equations of the form:<br>
 * <br>
 * x^2 - –Dy^2 = 1<br>
 * <br>
 * For example, when D=13, the minimal solution in x is 
 * 649^(2) + 13*180^(2) = 1.<br>
 * <br>
 * It can be assumed that there are no solutions in positive integers when D 
 * is square.<br>
 * <br>
 * By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the 
 * following:<br>
 * <br>
 * 3^2 - 2*2^2 = 1<br>
 * 2^2 - 3*1^2 = 1<br>
 * 9^2 - 5*4^2 = 1<br>
 * 5^2 - 6*2^2 = 1<br>
 * 8^2 - 7*3^2 = 1<br>
 * <br>
 * Hence, by considering minimal solutions in x for D <= 7, the largest x is 
 * obtained when D = 5.<br>
 * <br>
 * <b>Find the value of D <= 1000 in minimal solutions of x for which the 
 * largest value of x is obtained.</b><br>
 * 
 */
object Problem066 {
  
  import scala.actors.Futures._

  type Num = BigInt
  
  type Trio = (Num, Int, Num)
  
  val ONE = BigInt(1) 
  
  def testD(d: Int): Trio = {
    
    val bigD = BigInt(d)
    
    def getX(y: Num): (Num, Num) = {
      val x = Utils.getSqrt((ONE +  bigD * y * y).longValue)
      if(x.isRight) (BigInt(x.right.get), y)
      else getX(y + 1)
    }
    
    val t0 = System.currentTimeMillis
    val (x, y) = getX(ONE)
    val deltaT = System.currentTimeMillis - t0
    println("(" + d + ", " + y + ", " + x + ") => " + deltaT + " ms")
    (x, d, y)
  }
  
  def getMaxX(list: List[Trio]): Trio = {
    
    def getMax(lst: List[Trio], maxX: Trio): Trio = lst match {
      case Nil => maxX
      case first :: others if(first._1 > maxX._1) => getMax(others, first)
      case first :: others => getMax(others, maxX)
    }
    
    getMax(list.tail, list.first)
  } 
  

  
  def main(args : Array[String]) : Unit = {
    val max = 100
    
    val t0 = System.currentTimeMillis
    val result = (1 to max)
      .filter(Utils.getSqrt(_).isLeft)
      .map(i => future{testD(i)}).force.map(_())
      .toList
    val maxX = getMaxX(result)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println(maxX)
    println("Total Time: " + deltaT + " ms")
  }
}
