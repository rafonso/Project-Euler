package eulerProject

/**
 * Problem 188: The hyperexponentiation of a number<br>
 * 04 April 2008<br>
 * <br>
 * The hyperexponentiation or tetration of a number a by a positive integer b, 
 * denoted by a^^b or <sup>b</sup>a, is recursively defined by:<br>
 * <br>
 * a^^1 = a,<br>
 * a^^(k+1) = a<sup>a^^k</sup>.<br>
 * <br>
 * Thus we have e.g. 3^^2 = 3<sup>3</sup> = 27, hence 
 * 3^^3 = 3<sup>27</sup> = 7625597484987 
 * and 3^^4 is roughly 10<sup>3.6383346400240996*10^12<sup>.<br>
 * <br>
 * <b>Find the last 8 digits of 1777^^1855.</b><br>
 * <br>
 */
object Problem188 {
  def main(args : Array[String]) : Unit = {}
}

class Tetration(base: Int) {
  
  def ^^(expoent: Int): BigInt = {
    
    def pow(hiperExpoent: BigInt): BigInt = {
      0
    }
    
    def calculate(index: Int, accumulated: BigInt): BigInt = 
      if(index == expoent) accumulated
      else calculate(index + 1, pow(accumulated))
    
    calculate(1, 1)
  }
}

object Tetration {
  
}