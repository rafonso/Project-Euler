package projectEuler

object Problem108 extends App {

  def getDiophantFactors(n: Long): Seq[(Long, Long)] = {

      def getPair(x: Long, y: Long) = if (x < y) (x, y) else (y, x)

      def sortPair(pair1: (Long, Long), pair2: (Long, Long)) = (pair1._1 < pair2._1)

      def evaluate(x: Long, xMax: Long, solutions: List[(Long, Long)]): List[(Long, Long)] = {
        if (x > xMax) solutions.sortWith(sortPair)
        else {
          val num = n * x
          val den = x - n
          (num / den, num % den) match {
            case (y, 0) => evaluate(x + 1, xMax, getPair(x, y) :: solutions)
            case _      => evaluate(x + 1, xMax, solutions)
          }
        }
      }

    evaluate(n + 1, 2 * n, Nil)
  }

  def getDistinctSolutions(minQty: Long, n: Long = 1): Long = {
    val factors = getDiophantFactors(n)
    println("%,7d => %,7d".format(n, factors.size))

    if (factors.size > minQty) n
    else getDistinctSolutions(minQty, n + 1)
  }

  //  println(getDiophantFactors(4))
  //  
  //  (1 to args(0).toLong).foreach(n => println("%,7d => %s".format(n, getDiophantFactors(n))))
  //  
    getDistinctSolutions(args(0).toLong)
//    
//  (args(0).toLong to args(1).toLong).foreach(n => println("%,7d => %s".format(n, getDiophantFactors(n))))
}
