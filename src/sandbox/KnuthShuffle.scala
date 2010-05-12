package sandbox

object KnuthShuffle {
  
  def shuffle[A](array: Array[A]): Unit = {
    val random = new scala.util.Random
    
    for(n <- (array.size to 1 by -1)) {
      val pos = random.nextInt(n)
      val temp = array(pos)
      array(pos) = array(n - 1)
      array(n - 1) = temp
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val array = Array('a', 'b', 'c', 'd', 'e', 'f', 'g')
    
    println("before: " + array.toString)
    shuffle(array)
    println("after : " + array.toString)
  }
}
