package sandbox

object TestPermutations {
  
  def getPermutations[A](elements: List[A]): List[List[A]] = {
    
    def generate(currentElements: List[A], currentPermutation: List[A], permutations: List[List[A]]): List[List[A]] = currentElements match {
      case Nil => (currentPermutation.reverse :: permutations)
      case _ => //(0 until currentElements.size).flatMap(i => generate(currentElements.take(i) ::: currentElements.drop(i).tail, currentElements(i) :: currentPermutation, permutations)).toList
        currentElements.flatMap(a => generate(currentElements.remove(_ == a), a :: currentPermutation, permutations))
    }
    
    generate(elements, Nil, Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    val lista = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    
    println(getPermutations(lista).size)
  }
}
