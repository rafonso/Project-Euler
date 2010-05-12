package sandbox

object TestLisConbinations {
  
  def getCombinations[A](elements: List[A]): List[List[A]] = {
    
    def generate(list: List[A], previousCombinations: List[List[A]]): List[List[A]] = list match {
      case Nil => previousCombinations.reverse.map(_.reverse).reverse
      case x :: xs => {
        println(x + ": previous: " + previousCombinations)
        val currentCombinations = List(x) :: previousCombinations.map(combination => x :: combination)
        println(x + ": current : " + currentCombinations)
        println
        generate(xs, previousCombinations ::: currentCombinations)
      }
    }
    
    generate(elements , Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    
    println(getCombinations(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)).map(_.mkString).sort(_ < _).filter(_.size == 10))
  }
}
/*
List(
 * List(1), 
 * List(1, 2), 
 * List(1, 2, 3), 
 * List(1, 2, 3, 4), 
 * List(1, 2, 4), 
 * List(1, 3), 
 * List(1, 3, 4), 
 * List(1, 4), 
 * List(2), 
 * List(2, 3), 
 * List(2, 3, 4), 
 * List(2, 4), 
 * List(3), 
 * List(3, 4), 
 * List(4)
 )
 */