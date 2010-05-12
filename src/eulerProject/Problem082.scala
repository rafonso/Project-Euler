package eulerProject

/**
 * Problem 82: Find the minimal path sum from the left column to the right 
 * column.<br>
 * 05 November 2004<br>
 * <br>
 * NOTE: This problem is a more challenging version of Problem 81.<br>
 * <br>
 * The minimal path sum in the 5 by 5 matrix below, by starting in any cell in 
 * the left column and finishing in any cell in the right column, and only 
 * moving up, down, and right, is indicated in red and bold; the sum is equal 
 * to 994.<br>
 * <pre>	
 * 131	673	<span style="color:red">234</span>	<span style="color:red">103</span>	<span style="color:red">18</span>
 * <span style="color:red">201</span>	<span style="color:red">96</span>	<span style="color:red">342</span>	965	150
 * 630	803	746	422	111
 * 537	699	497	121	956
 * 805	732	524	37	331
 * </pre>	
 * <b>Find the minimal path sum, in matrix.txt, a 31K text file containing a 
 * 80 by 80 matrix, from the left column to the right column.</b><br>
 * 
 */
object Problem082 {
  
  import RectangularPath._
  
  class LeftToRightPathCalculator extends PathCalculator {
    
    def getMinimalPath(matrix: Matrix): PathValue = {
      
      val maxRow = matrix.size - 1
      val maxCol = matrix(0).size - 1
      
      def calculatePath1(nextRow: Int, nextCol: Int, current: PathValue, smallest: PathValue): PathValue = {
        if(current.containsPonit(nextRow, nextCol)) smallest
        else if((nextRow >= 0) && (nextRow <= maxRow)) calculatePath(current.next(nextRow, nextCol, matrix(nextRow)(nextCol)), smallest)
        else smallest
      }
      
      def calculatePath(currentPath: PathValue, smallestPath: PathValue): PathValue = {
        if(currentPath.value > smallestPath.value) {
          smallestPath
        } else if(currentPath.currentColumn == maxCol) {
          Utils.log('\t' + "Smallest: " + currentPath)
          currentPath
        } else {
          val smallestPathRowUp       = calculatePath1(currentPath.currentRow - 1, currentPath.currentColumn    , currentPath, smallestPath)
          val smallestPathColumnRight = calculatePath1(currentPath.currentRow    , currentPath.currentColumn + 1, currentPath, smallestPathRowUp)
          calculatePath1(currentPath.currentRow + 1, currentPath.currentColumn, currentPath, smallestPathColumnRight)
        }
      }
      
      def getInitialPathForRow(row: Int) = PathValue(row, 0, matrix(row)(0), List(Point(row, 0, matrix(row)(0))))
      
      (0 to maxRow).foldLeft(initialPath)((smallest, row) => {
        Utils.log("Começando da linha " + row.toString)
        calculatePath(getInitialPathForRow(row), smallest)
      })
    }
    
  }
  
  val strMatrix1 = """
131 673 234 103 18
201 96 342 965 150
630 803 746 422 111
537 699 497 121 956
805 732 524 37 331
""".trim

  
  def main(args : Array[String]) : Unit = {
    val (path, time) =  evaluatePath(getMatrixFromFile, new LeftToRightPathCalculator)
    
    println("==================")
    println(path)
    println("Time = " + time + " ms")
  }
}
