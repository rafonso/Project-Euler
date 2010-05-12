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
object Problem173a {
  
  import Utils._
  
  def showTiles(currentTiles: Int, k: Int, l: Int, odd: Boolean) {
    val i = 2 * k + (if(odd) 1 else 0)
    val j = 2 * l + (if(odd) 1 else 0)
    println("%,9d => %,7d until %,7d (k = %,7d, l = %,7d, odd = %s)".format(currentTiles, i, j, k, l, odd))
  }
  
  def getL(k: Int, odd: Boolean, tiles: Int): Option[Int] = {
    val d = if(odd) 1 else 0
    val parcel = 2 * k + d - 1
    getSqrt(tiles + parcel * parcel) match {
      case Right(root) => {
        val (l, rem) = /%(root - d, 2)
        if(rem == 0) Some(l.toInt)
        else None
      }
      case Left(_) => None
    }
  }
  
  def evaluateTilesK(k: Int, odd: Boolean, tiles: Int, kMax: Int, qty: Int): Int = {
    if(k <= kMax) {
      val nextQty = getL(k, odd, tiles) match {
        case Some(l) => {
          showTiles(tiles, k, l, odd)
          qty + 1
        } 
        case None => qty
      }
      val nextK = if(odd) k else k + 1
      evaluateTilesK(nextK, !odd, tiles, kMax, nextQty)
    } else {
      qty
    }
  }
  
  def main(args : Array[String]) : Unit = {
    /*
    val max = 100
    
    val t0 = System.currentTimeMillis
    val result = (1 to max).foldLeft(0)((sum, tiles) => sum + evaluateTilesK(1, false, tiles, tiles / 8, 0))
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
    */
    println(evaluateTilesK(0, false, 9, 9, 0))
  }
}
