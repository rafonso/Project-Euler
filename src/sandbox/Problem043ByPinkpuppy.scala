package sandbox

/**
 * ver http://projecteuler.net/index.php?section=forum&id=43&page=5
 */
object Problem043ByPinkpuppy {
  
  val divs = List(1,1,17,13,11,7,5,3,2,1)
  
  val digits = (0 to 9).toList
  
  def toNum(xs: Seq[Int]) = xs.foldLeft(0L){case (s,i) => 10*s+i}
  
  def rest(xs: List[Int]) = digits -- xs
  
  def mod(xs: List[Int], div:Int) = toNum(xs.take(3))%div == 0
  
  def doit(divs: Seq[Int]) =
      divs.foldLeft(List(List[Int]())) {(terms,div) =>
        for (t <- terms; i <- rest(t); val poss = i::t if mod(poss,div)) yield poss
      }
  
  def apply() = doit(divs).map(toNum(_)).foldLeft(0L)(_+_)

  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val sum = apply
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(sum)
    println("Total Time: " + deltaT + " ms")
  }
}
