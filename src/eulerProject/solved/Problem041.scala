package projectEuler

/**
 *
 * Problem 41.<br/>
 * 11 April 2003.<br/>
 * <br/>
 * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For 
 * example, 2143 is a 4-digit pandigital and is also prime.<br/>
 * <br/>
 * <b>What is the largest n-digit pandigital prime that exists?</b>
 *
 */
object Problem041 {

  def isPandigital(n: Long): Boolean = {
    val size = n.toString.size

      def evaluate(x: Long, digits: Array[Boolean]): Boolean = x match {
        case 0 => {
//          println(n)
          true
        }
          //digits.forall(_ == true)
        case _ => (x % 10) match {
          case 0                    => false
          case d if (d > size)      => false
          case d if (digits(d.toInt - 1)) => false
          case d => {
            digits(d.toInt - 1) = true
            evaluate(x / 10, digits)
          }
        }
      }

    if (size > 9) false else evaluate(n, Array.fill(size)(false))
  }
  
  

  def main(args: Array[String]): Unit = {
    new PrimesGenerator().takeUntil(1000000000).filter(isPandigital).foreach(println)

  }
}
