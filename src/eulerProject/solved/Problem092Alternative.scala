package eulerProject.solved

object Problem092Alternative {
  
  val squares = Array(0, 1, 4, 9, 16, 25, 36, 49, 64, 81)
  
  def getDigitsSquare(x: Int): Int = {
    
    def eval(n: Int, sum: Int): Int = n match {
      case 0 => sum
      case _ => {
        val (div, digit) = eulerProject.Utils./%(n, 10)
        eval(div, sum + squares(digit))
      }
    }
    
    eval(x, 0)
  }
  
  def getNumbers89Until(limit: Int): Int = {
    
    def is89(x: Int): Boolean = {
      val result = getDigitsSquare(x) 
      result match {
        case 1 => false
        case 89 => true
        case _ => is89(result)
      }
    }
    
    def evalN(n: Int, qty89: Int): Int = {
      if(n % 1000 == 0) println(n)
      if(n > limit) qty89
      else if(is89(n)) evalN(n + 1, qty89 + 1)
      else evalN(n + 1, qty89)
    }
    
    evalN(2, 0)
  }  

  
  def main(args : Array[String]) : Unit = {
    val max = 10000000
    
    val t0 = System.currentTimeMillis
    val result = getNumbers89Until(max)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
