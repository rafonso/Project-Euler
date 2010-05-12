package eulerProject

/**
 * Problem 209: <br>
 * 19 September 2008<br>
 * <br>
 * A k-input binary truth table is a map from k input bits (binary digits, 
 * 0 [false] or 1 [true]) to 1 output bit. For example, the 2-input binary 
 * truth tables for the logical AND and XOR functions are:<br>
 * <table border="1">
 *  <tr>
 *   <th>x</th>
 *   <th>y</th>
 *   <th>x AND y</th>
 *  </tr>
 *  <tr>
 *   <td>0</td>
 *   <td>0</td>
 *   <td>0</td>
 *  </tr>
 *  <tr>
 *   <td>0</td>
 *   <td>1</td>
 *   <td>0</td>
 *  </tr>
 *  <tr>
 *   <td>1</td>
 *   <td>0</td>
 *   <td>0</td>
 *  </tr>
 *  <tr>
 *   <td>1</td>
 *   <td>1</td>
 *   <td>1</td>
 *  </tr>
 * </table>
 * 
 * <table border="1">
 *  <tr>
 *   <th>x</th>
 *   <th>y</th>
 *   <th>x XOR y</th>
 *  </tr>
 *  <tr>
 *   <td>0</td>
 *   <td>0</td>
 *   <td>0</td>
 *  </tr>
 *  <tr>
 *   <td>0</td>
 *   <td>1</td>
 *   <td>1</td>
 *  </tr>
 *  <tr>
 *   <td>1</td>
 *   <td>0</td>
 *   <td>1</td>
 *  </tr>
 *  <tr>
 *   <td>1</td>
 *   <td>1</td>
 *   <td>0</td>
 *  </tr>
 * </table>
 * 
 * How many 6-input binary truth tables, tau, satisfy the formula<br>
 * tau(a, b, c, d, e, f) AND tau(b, c, d, e, f, a XOR (b AND c)) = 0<br>
 * for all 6-bit inputs (a, b, c, d, e, f)?<br>
 * 
 */
object Problem209 {
  
  case class Binary(digit: Byte) {
    def and(other: Binary): Binary = 
      if(this == ONE && other == ONE) ONE else ZERO
    
    def xor(other: Binary): Binary = 
      if(this == other) ZERO else ONE
    
    override def toString = this.digit.toString
  }
  
  object ONE extends Binary(1) 
  
  object ZERO extends Binary(0)
  
  val binaries = Array(ZERO, ONE)
  
  def show(a: Binary, b: Binary, c: Binary) = {
    println(a + " XOR (" + b + " AND " + c + ") = " + a.xor(b.and(c)))
  }
  
  def main(args : Array[String]) : Unit = {
    for(a <- binaries; b <- binaries; c <- binaries) show(a, b, c)
    
  }
}
