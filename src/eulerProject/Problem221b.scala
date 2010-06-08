package eulerProject

/**
 * Problem 221: Alexandrian Longegers<br>
 * 13 December 2008<br>
 * <br>
 * We shall call a positive integer A an "Alexandrian integer", if there 
 * exist integers p, q, r such that:<br>
 * <br>
 * A = p * q * r    and   1 / A = 1 / p + 1 / q + 1 / r <br>
 * <br>
 * For example, 630 is an Alexandrian integer (p = 5, q = -7, r = -18). 
 * In fact, 630 is the 6^(th) Alexandrian integer, the first 6 Alexandrian 
 * integers being: 6, 42, 120, 156, 420 and 630.<br>
 * <br>
 * <b>Find the 150000<sup>th</sup> Alexandrian integer.</b><br>
 * <br>
 * <hr>
 * <pre>
 * p * q * r = A (1)
 * 1/p + 1/q + 1/r = 1/A (2)
 * From (2):
 * (q * r + p * r + p * q)/(p * q * r) = 1 / A (3)
 * So
 * p * q + p * r + q * r = p * (q + r) + q * r = p * s + q * r = 1 (4)
 * Where 
 * s = q + r (5)
 * From (1)
 * q * r = A / p  = d (6)
 * Appling (6) in (4)
 * p * s + d = 1 (7)
 * s = (p - A) / p ^ 2 (8)
 * How p > 0, A > 0 and A > p
 * s < 0 (9)
 * From (6), for (A / p) be a integer, p must be a divisor of A
 * - pMax:
 * From (8):
 * s <= -1 <--> (p - A) / p ^ 2 <= -1 <--> p - A <= - p ^ 2 <--> p ^ 2 + p - A <= 0
 * <--> p <= (-1 + sqrt(1 + 4 * A)) / 2
 * For A >> 0, sqrt(1 + 4 * A) ~= sqrt(4 * A). So:
 * p <= - (1 + sqrt(4 * A)) / 2 ~ - (1 + 2 * sqrt(A)) / 2 = sqrt(A) - 1 / 2
 * So we may consider pMax = sqrt(A)
 * - Getting q
 * From (1)
 * r = A / (p * q) (10)
 * From (5)
 * s = q + r <--> s = q + A / (p * q) <--> p * q ^ 2 - p * s * q + A = 0
 * To solve this equation:
 * delta = p ^ 2 * s ^2 - 4 * p * A (11)
 * With delta >= 0
 * q = ((p * s) +- sqrt(delta)) / (2 * p) (12)
 * 
 * 
 * Para resumir:
 * -> Para um determinado A > 0:
 * -> p é divisor de A
 * -> p < sqrt(A)
 * -> s = q + r = (p - A) / p ^ 2 < 0
 * -> delta = p * 2 * s ^ 2 - 4 * p * A > 0 
 * -> q = ((p * s) +- sqrt(delta)) / (2 * p) < 0 
 * -> r = A / (p * q) < 0 
 * -> todos os valores devem ser inteiros.
 * <pre>
 */
object Problem221b {
  
  import Utils._
  
  type Alexandrian = (Long, Long, Long)
  
  def isAlexandrian(a: Long): Option[Alexandrian] = {
    
    def getQ(p: Long, s: Long, deltaRoot: Long): Option[Alexandrian] = (p * s - deltaRoot) /% (2 * p) match {
      case (q, 0) => Some((p, q, s - q))
      case _ => None
    }
    
    def qValid(p: Long, s: Long): Option[Alexandrian] = (p * p * s * s - 4 * p * a) match {
      case delta if (delta == 0) => getQ(p, s, 0)
      case delta if (delta >  0) => getSqrt(delta) match {
        case Right(deltaRoot) => getQ(p, s, deltaRoot)
        case _ => None
      }
      case _ => None
    }
    
    def getS(p: Long): Option[Long] = (p - a) /% (p * p) match {
      case (s, 0) => Some(s)
      case _ => None
    }
    
    def evalP(p: Long, pMax: Long): Option[Alexandrian] = {
      if(p > pMax) {
        None
      } else if(a % p == 0) {
        getS(p) match {
          case Some(s) => qValid(p, s) match {
            case Some(alexandrian)  => Some(alexandrian)
            case None => evalP(p + 1, pMax)
          }
          case _ => evalP(p + 1, pMax)
        }
      } else {
        evalP(p + 1, pMax)
      }
    }
    
    val pMax = Math.sqrt(a).toLong
    evalP(1, pMax)
  }
  
  def getNthAlexandrian(quantity: Int): Long = {
    
    def getNth(a: Long, qty: Int): Long = isAlexandrian(a) match {
      case Some((p, q, r)) => {
        val nextQty = qty + 1
        log("%,7d: A = %,14d, p = %,7d, q = %,7d, r = %,7d".format(nextQty, a, p, q, r))
        if(nextQty >= quantity) a
        else getNth(a + 2, nextQty)
      } 
      case None => getNth(a + 2, qty)
    }
    
    getNth(2, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = getNthAlexandrian(max)
      //isAlexandrian(6)
      //getNthAlexandrian(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
