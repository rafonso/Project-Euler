package eulerProject

/**
 * Problem 173: <br/>
 * 22 December 2007<br/>
 * <br/>
 * We shall define a square lamina to be a square outline with a square "hole" 
 * so that the shape possesses vertical and horizontal symmetry. For example, 
 * using exactly thirty-two square tiles we can form two different square 
 * laminae:<br/>
 * <div style="text-align: center;">
 * <img src="http://projecteuler.net/project/images/p_173_square_laminas.gif" alt="">
 * </div>
 * <br/>
 * With one-hundred tiles, and not necessarily using all of the tiles at one 
 * time, it is possible to form forty-one different square laminae.<br/>
 * <br/>
 * <b>Using up to one million tiles how many different square laminae can 
 * be formed?</b><br/>
 * <br/>
 */
object Problem173 {
  
  import scala.collection.immutable._
  
  type Lamina = (Int, Int)
  
  def getSquares(laminae: List[Lamina], tiles: Int): Int = {

    def evaluate(currentLaminae: List[Lamina], remaingLaminae: List[Lamina], currentTiles: Int, quantity: Int): Int = {
      if(currentTiles > tiles) {
        val lastLamina = currentLaminae.last
        evaluate(currentLaminae.init, remaingLaminae, currentTiles - lastLamina._2, quantity)
      } else if(currentTiles == tiles) {
        println(currentLaminae.reverse)
        remaingLaminae match {
          case lamina :: others => {
            val lastLamina = if(currentLaminae.isEmpty) (0, 0) else currentLaminae.last
            evaluate(lamina :: currentLaminae.init, others, lamina._2 + currentTiles - lastLamina._2, quantity + 1)
          }
          case Nil => quantity + 1
        }
      } else { // currentTiles < max
        remaingLaminae match {
          case lamina :: others => {
            val lastLamina = if(currentLaminae.isEmpty) (0, 0) else currentLaminae.last
            evaluate(lamina :: currentLaminae, others, lamina._2 + currentTiles, quantity)
          }
          case Nil => quantity
        }
      }
    }
    
//    println(tiles + " => " + laminae)
    evaluate(Nil, laminae, 0, 0)
  }
  
  def getLaminae(max: Int): List[Lamina] = (2 to (max / 4 + 1)).map(i => (i, 4 * (i - 1))).toList
  
  def isLaminaOdd(l : Lamina) = (l._1 % 2 == 0)
  
  def countSquares(max: Int): Int = {
    
    def isOdd(n: Int) = (n % 2 == 0)
    
    def countForTiles(tiles: Int, disponibleLaminae: List[Lamina]): Int = {
      getSquares(disponibleLaminae.filter(isLaminaOdd(_)), tiles) + 
        getSquares(disponibleLaminae.filter(!isLaminaOdd(_)), tiles)
    }
    
    val laminae = getLaminae(max)
    (1 to max).foldLeft(0)((sum, tiles) => sum + countForTiles(tiles, laminae.takeWhile(_._2 <= tiles)))
  }
  
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = countSquares(max)
      //getSquares(getLaminae(max).filter(isLaminaOdd(_)), max)
      //countSquares(max) 
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
