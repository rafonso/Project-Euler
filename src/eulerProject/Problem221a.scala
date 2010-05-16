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
 * q * r = A / p (6)
 * Appling (6) in (4)
 * p * s + A / p = 1 (7)
 * s = (p - A) / p ^ 2 (8)
 * How p > 0, A > 0 and A > p
 * s < 0 (9)
 * From (6), for (A / p) be a integer, p must be a divisor of A
 * 
 * Para resumir
 * 
 * -> p é divisor de A
 * -> s = q + r = (p - A) / p^2, deve ser um inteiro negativo
 * -> Não é o foco encontrar os valores de q e r. apenassaber que eles existem.
 * <pre>
 */
object Problem221a {
  
  type Alexandrian = (Long, Long, Long, Long)
  
  import Utils._
  
  def getQR(s: Long, d: Long): Option[(Long, Long)] = {
    val delta = s * s - 4 * d
    var result: Option[(Long, Long)] = None
    if(delta > 0) {
      val rootDelta = getSqrt(delta)
      if(rootDelta.isRight) {
        val (q, remQ) = /%(s + rootDelta.right.get, 2)
        val (r, remR) = /%(s - rootDelta.right.get, 2)
        if(remQ == 0 && remR == 0) {
          result = Some((q, r))
        } 
      } 
    } 
    result
  }
  
  def testP(A: Long, p: Long): Option[Alexandrian] = {
    val (s, remainder) = /%(p - A, p * p)
    var result: Option[Alexandrian] = None
    if(remainder == 0) {
      val qr = getQR(s, A / p)
      if(qr.isDefined) {
        result = Some((A, p, qr.get._1, qr.get._2))
      } 
    } 
    result
  }
  
  def evaluateA(A: Long, candidate: Long, limitCandidate: Long): Option[Alexandrian] = {
    if(candidate > limitCandidate) {
      None
    } else {
      val (quocient, remainder) = /%(A, candidate)
      if(remainder == 0) {
        val alex = testP(A, candidate)
        if(alex.isDefined) {
          alex
        } else {
          val alex1 = testP(A, quocient)
          if(alex1.isDefined) {
            alex1
          } else {
            evaluateA(A, candidate + 1, limitCandidate)
          }
        }
      } else {
        evaluateA(A, candidate + 1, limitCandidate)
      }
    }
  }
  
  def getNthAlexandrian(quantity: Int): Long = {
    
    def eval(A: Long, qty: Int): Long = {
      val alex = evaluateA(A, 1, Math.sqrt(A.toDouble).toLong)
      if(alex.isDefined) {
        val nextQty = qty + 1
        log("%,7d -> %,11d, %,7d, %,7d, %,7d".format(nextQty, alex.get._1, alex.get._2, alex.get._3, alex.get._4))
        if(nextQty == quantity) {
          A
        } else {
          eval(A + 1, nextQty)
        }
      } else {
        eval(A + 1, qty)
      }
    }
    
    eval(1, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = getNthAlexandrian(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
