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
object Problem221c {
  
  import Utils._
  
  def evaluateA(a: Int): Boolean = {
    
    def showResult(l: Long, p: Long, q: Long) = {
      log("A = %,11d, p = %,11d, q = %,11d, l = %,11d".format(a, p, q, l))
      true
    }
    
    def evaluateL(l: Long, lMax: Long): Boolean = {
      if(l <= lMax) {
        getSqrt(4 * a * l + 1) match {
          case Right(rootP) => {
            ((- 1 - rootP) /% (2 * l)) match {
              case (p, 0) if((l * l) > (4L * a * p)) => {
                getSqrt(l * l - 4 * p * a) match {
                  case Right(deltaQ) => {
                    (-1 - deltaQ) /% (2 * p) match {
                      case (q, 0) => showResult(l, p, q)
                      case _ => {
                        (-1 + deltaQ) /% (2 * p) match {
                          case (q, 0) => showResult(l, p, q)
                          case _ => evaluateL(l + 1, lMax)
                        }
                      }
                    }
                  }
                  case _ => evaluateL(l + 1, lMax)
                }
                /*
                log("l^2 = " + (l * l) + ", 4A = " + (4 * a))
                true
                getSqrt(l * l - 4 * p * a) match {
                  case Right(root1) => {
                    (- l - root1) /% (2 * p) match {
                      case (q, 0) => showResult(l, p, q)
                      case _ => {
                        (- l + root1) /% (2 * p) match {
                          case (q, 0) => showResult(l, p, q)
                          case _ => evaluateL(l + 1, lMax)
                        }
                      }
                    }
                  }
                  case _ => evaluateL(l + 1, lMax)
                }
                    */
              }
              case _ => evaluateL(l + 1, lMax)
            }
          }
          case _ => evaluateL(l + 1, lMax)
        }
      } else {
        false
      }
    }
    
    val aMin = 2 * Math.sqrt(a).toLong
    val lMax = a - 1
    evaluateL(aMin, lMax)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = evaluateA(630)
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
