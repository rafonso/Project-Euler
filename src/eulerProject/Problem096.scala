package eulerProject

/**
 * Problem 96: Devise an algorithm for solving Su Doku puzzles.<br>
 * 
 * <p>Su Doku (Japanese meaning <i>number place</i>) is the name given to a 
 * popular puzzle concept. Its origin is unclear, but credit must be attributed 
 * to Leonhard Euler who invented a similar, and much more difficult, puzzle 
 * idea called Latin Squares. The objective of Su Doku puzzles, however, is to 
 * replace the blanks (or zeros) in a 9 by 9 grid in such that each row, 
 * column, and 3 by 3 box contains each of the digits 1 to 9. Below is an 
 * example of a typical starting puzzle grid and its solution grid.</p>
 * 
 * <div style="text-align: center; font-family: courier new; font-size: 14pt;">
 * <table align="center" border="0" cellpadding="0" cellspacing="0">
 * <tbody><tr>
 * <td>
 * <table border="1" cellpadding="5" cellspacing="0">
 * <tbody><tr>
 * <td>0 0 3<br>9 0 0<br>0 0 1</td>
 * <td>0 2 0<br>3 0 5<br>8 0 6</td>
 * <td>6 0 0<br>0 0 1<br>4 0 0</td>
 * </tr>
 * <tr>
 * <td>0 0 8<br>7 0 0<br>0 0 6</td>
 * <td>1 0 2<br>0 0 0<br>7 0 8</td>
 * <td>9 0 0<br>0 0 8<br>2 0 0</td>
 * </tr>
 * <tr>
 * <td>0 0 2<br>8 0 0<br>0 0 5</td>
 * <td>6 0 9<br>2 0 3<br>0 1 0</td>
 * <td>5 0 0<br>0 0 9<br>3 0 0</td>
 * </tr>
 * </tbody>
 * </table>
 * </td>
 * 
 * <td width="50"><img src="images/spacer.gif" alt="" height="1" width="50"><br></td>
 * <td>
 * <table border="1" cellpadding="5" cellspacing="0">
 * <tbody><tr>
 * <td>4 8 3<br>9 6 7<br>2 5 1</td>
 * <td>9 2 1<br>3 4 5<br>8 7 6</td>
 * <td>6 5 7<br>8 2 1<br>4 9 3</td>
 * </tr>
 * <tr>
 * <td>5 4 8<br>7 2 9<br>1 3 6</td>
 * <td>1 3 2<br>5 6 4<br>7 9 8</td>
 * <td>9 7 6<br>1 3 8<br>2 4 5</td>
 * </tr>
 * <tr>
 * <td>3 7 2<br>8 1 4<br>6 9 5</td>
 * <td>6 8 9<br>2 5 3<br>4 1 7</td>
 * <td>5 1 4<br>7 6 9<br>3 8 2</td>
 * </tr>
 * </tbody></table>
 * </td>
 * </tr>
 * </tbody></table>
 * </div>
 * 
 * <p>A well constructed Su Doku puzzle has a unique solution and can be 
 * solved by logic, although it may be necessary to employ "guess and test" 
 * methods in order to eliminate options (there is much contested opinion over 
 * this). The complexity of the search determines the difficulty of the 
 * puzzle; the example above is considered <i>easy</i> because it can be 
 * solved by straight forward direct deduction.</p>
 * 
 * <p>The 6K text file, <a href="project/sudoku.txt">sudoku.txt</a> (right 
 * click and 'Save Link/Target As...'), contains fifty different Su Doku 
 * puzzles ranging in difficulty, but all with unique solutions (the first 
 * puzzle in the file is the example above).</p>
 * 
 * <p><b>By solving all fifty puzzles find the sum of the 3-digit numbers found 
 * in the top left corner of each solution grid; for example, 483 is the 
 * 3-digit number found in the top left corner of the solution grid above.</b></p>
 * 
 * <!--<p class='info'>Note: If you're convinced that 
 * &quot;guess and test&quot; methods need not be employed please tell how 
 * you would solve #6 and #46. (c;</p>-->
 * 
 */
object Problem096 {
  
  import Utils._
  
  def main(args : Array[String]) : Unit = {
    val max = 1000
    
    val t0 = System.currentTimeMillis
    val result = max
    val deltaT = System.currentTimeMillis - t0
    
    println("=" * 80)
    log(result)
    log("Total Time: " + deltaT + " ms")
  }
}
