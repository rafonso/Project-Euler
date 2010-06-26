package eulerProject.solved

/**
 * Problem 142: Perfect Square Collection<br>
 * 24 February 2007<br>
 * <br>
 * <b>Find the smallest x + y + z with integers x > y > z > 0 such that x + y, 
 * x - y, x + z, x - z, y + z, y - z are all perfect squares.</b><br>
 * <pre>
 * sumXY  = x + y = n^2 (1)
 * diffXY = x - y (2)
 * From (1) and (2)
 * sumXY + diffXY = 2*x
 * 
 * x = (sumXY + diffXY) / 2 (3)
 * y = sumXY - x (3)
 * 
 * x + z = sumXZ (4)
 * y + z = sumYZ (5)
 * Adding (4) to (5)
 * x + y + 2 * z = sumXY + sumYZ
 * From (1)
 * sumXY + 2 * z = sumXY + sumYZ
 * z = (sumXY + sumYZ - sumXY) / 2
 * </pre>
 */
object Problem142a {
  
  import Utils._
    
  type Triplet = (Long, Long, Long)
  type OptTriplet = Option[Triplet]
  
  def evaluateN(n: Long, squares: List[Long]): Triplet  = {
    val sumXY = n * n
    
    def evaluateSumYZ(x: Long, y: Long, sumXZ: Long, squaresSumYZ: List[Long]): Option[Long] = squaresSumYZ match {
      case Nil => None
      case sumYZ :: others if(sumYZ < y) => None
      case sumYZ :: others if(sumXZ + sumYZ <= sumXY) => None
      case sumYZ :: others => {
        (sumXZ + sumYZ - sumXY) /% 2 match {
          case (z, 0) if((x + z == sumXZ) && (y + z == sumYZ)) => {
//            println("\tx = " + x + ", y = " + y + ", z = " + z + ", sumXZ = "+ sumXZ + ", sumYZ = " + sumYZ)
            if(squares.contains(x - z) && squares.contains(y - z)) Some(z)
            else evaluateSumYZ(x, y, sumXZ, others)
          }
          case _ => evaluateSumYZ(x, y, sumXZ, others)
        }
      }
    }
    
    def evaluateSumXZ(x: Long, y: Long, squaresSumXZ: List[Long]): Option[Long] = squaresSumXZ match {
      case Nil => None
      case sumXZ :: others if(sumXZ < x) => None
      case sumXZ :: others => {
        evaluateSumYZ(x, y, sumXZ, others) match {
          case Some(z) => Some(z)
          case None => evaluateSumXZ(x, y, others)
        }
      }
    }
    
    def evaluateDiffXY(squaresDiffXY: List[Long]): OptTriplet = squaresDiffXY match {
      case Nil => None
      case diffXY :: others => {
        (sumXY + diffXY) /% 2 match {
          case (x, 0) => {
            val y = sumXY - x
            evaluateSumXZ(x, y, squares) match {
              case Some(z) => Some((x, y, z))
              case None => evaluateDiffXY(others)
            }
          }
          case _ => evaluateDiffXY(others)
        }
      }
    }
    
    log("Evaluating %,10d (%,20d)".format(n, sumXY))
    evaluateDiffXY(squares) match {
      case Some(triplet) => triplet
      case None => evaluateN(n + 1, sumXY :: squares)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val result = evaluateN(2L, List(1))
    val sum = result._1 + result._2 + result._3
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("SUM = " + sum)
    log("Total Time: " + deltaT + " ms")
  }

}
