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
 */
object Problem221 {
  
  type alexandrian = (Long, Long, Long, Long)
  type optAlexandrian = Option[alexandrian]
  
  def isAlexandrian(a: Long): optAlexandrian = {
    
    def getQ(a: Long, p: Long, sqrtDelta: Long, plus: Boolean): Option[Long] = {
      val num = p - a + (if(plus) sqrtDelta else (- sqrtDelta))
      val den = 2 * p * p
      if(num == 0) None
      else if(num % den == 0) Some(num / den) else None
    }
    
    def evalP(p: Long): optAlexandrian = {
//      println()
//      print("p = " + p + " ")
      if(p >= a / 2) {
        None
      } else {
        val delta = a * a - 2 * a * p + p * p - 4 * a * p * p * p
        if(delta >= 0) {
          val sqrt = Math.sqrt(delta).toLong
          if(sqrt * sqrt == delta) { // delta is quadratic?
//          print("sqrtDelta = " + sqrtDelta.get + " ")
            var q = getQ(a, p, sqrt, true)
            if(q.isDefined) {
//              print("q = " + q.get)
              if(a % (p * q.get) == 0) Some((a, p, q.get, a / (p * q.get)))
              else evalP(p + 1)
            } else {
              q = getQ(a, p, sqrt, false)
              if(q.isDefined) {
//                print("q = " + q.get)
                if(a % (p * q.get) == 0) Some((a, p, q.get, a / (p * q.get)))
                else evalP(p + 1)
              } else {
                evalP(p + 1)
              }
            }
          } else {
            evalP(p + 1)
          }
        } else {
          None
        }
      }
    }
    
    evalP(1)
  }
  
  def getNthAlexandrian(max: Long): Long = {
    
    def eval(a: Long, quant: Long): Long = {
      val alex = isAlexandrian(a)
      if(alex.isDefined) {
        val newQty = quant + 1
        printf("%,7d = %s%n", newQty, alex.get)
        if(newQty == max) alex.get._1
        else eval(a + 1, newQty)
      } else {
        eval(a + 1, quant)
      }
    }
    
    eval(1, 0)
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 200
    
    val t0 = System.currentTimeMillis
    val result = getNthAlexandrian(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
