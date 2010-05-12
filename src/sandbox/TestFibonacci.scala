package sandbox

object TestFibonacci {
  
  val ZERO = BigInt(0)
  val ONE  = BigInt(1)
  
  def iteractive(max: Int): BigInt = {
    var value1 = ZERO
    var value2 = ONE
    var value =  ONE
    
    for(i <- 3 to max) {
      value = value1 + value2
      value2 = value1
      value1 = value
    }
    
    value
  }
  
  def tailRecursive(max: Int): BigInt = {
    
    def calculate(value: BigInt, value1: BigInt, counter: Int): BigInt = {
      if(counter < max) calculate(value + value1, value, counter + 1)
      else value
    }
    
    calculate(1, 0, 3)
  }
  
  def nonTailRecursive(max: Int): BigInt = {
    
    def calculate(i: Int): BigInt = i match {
      case 0 | 1 => i
      case _ => calculate(i - 1) + calculate(i - 2)
    }
    
    calculate(max - 1) + calculate(max - 2)
  }
  
  def showResults(max: Int) {
    
    def show(f: Int => BigInt) {
      val t0 = System.currentTimeMillis
      val result = f(max)
      val deltaT = System.currentTimeMillis - t0
      val strResult = if(result.toString.size > 9) result.toString.substring(0, 9) else result.toString
      print("%9s |%9d |".format(strResult, deltaT))
    }
    
    print("%9d |".format(max))
    
    show(iteractive)
    show(tailRecursive)
    show(nonTailRecursive)
    
    println
  }
  
  def showHeader {
    
    def show(str: String) = print("%20s |".format(str))
    
    print(" " * 10)
    print("|")
    
    show("Iteractive")
    show("Tail Recursive")
    show("Non Tail Recursive")
    
    println
  }
  
  def main(args : Array[String]) : Unit = {
    showHeader
//    Array(3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000, 1000000).foreach(showResults(_))
    Array(3, 5, 10, 15, 20, 30, 35).foreach(showResults(_))
  }
}
