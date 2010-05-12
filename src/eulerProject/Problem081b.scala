package eulerProject

/**
 * Problem 81: Find the minimal path sum from the top left to the bottom right 
 * by moving right and down.<br>
 * 22 October 2004<br>
 * <br>
 * In the 5 by 5 matrix below, the minimal path sum from the top left to the 
 * bottom right, by only moving to the right and down, is indicated in bold 
 * red and is equal to 2427.<br>
 * <pre>	
 * <span style="color:red">131</span>	673	234	103	18
 * <span style="color:red">201</span>	<span style="color:red">96</span>	<span style="color:red">342</span>	965	150
 * 630	803	<span style="color:red">746</span>	<span style="color:red">422</span>	111
 * 537	699	497	<span style="color:red">121</span>	956
 * 805	732	524	<span style="color:red">37</span>	<span style="color:red">331</span>
 * </pre>	
 * <b>Find the minimal path sum, in matrix.txt (right click and 
 * 'Save Link/Target As...'), a 31K text file containing a 80 by 80 matrix, 
 * from the top left to the bottom right by only moving right and down.</b><br>
 */
object Problem081b {
  
  import RectangularPath._
  
  class LeftToRightDownPathCalculator extends PathCalculator {
    
    def getMinimalPath(matrix: Matrix): PathValue = {
      
      val maxRow = matrix.size - 1
      val maxCol = matrix(0).size - 1
      
      def calculatePath1(nextRow: Int, nextCol: Int, current: PathValue, smallest: PathValue): PathValue = {
        if(current.containsPonit(nextRow, nextCol)) smallest
        else if((nextRow >= 0) && (nextRow <= maxRow) && (nextCol >= 0) && (nextCol <= maxCol)) calculatePath(current.next(nextRow, nextCol, matrix(nextRow)(nextCol)), smallest)
        else smallest
      }
      
      def calculatePath(currentPath: PathValue, smallestPath: PathValue): PathValue = {
        if(currentPath.value > smallestPath.value) {
          Utils.log("- Rejected: " + currentPath)
          smallestPath
        } else if((currentPath.currentColumn == maxCol) && (currentPath.currentRow == maxRow)) {
          Utils.log("* Smallest: " + currentPath)
          currentPath
        } else {
          val smallestPathColumnRight = calculatePath1(currentPath.currentRow, currentPath.currentColumn + 1, currentPath, smallestPath)
          calculatePath1(currentPath.currentRow + 1, currentPath.currentColumn, currentPath, smallestPathColumnRight)
        }
      }
      
      calculatePath(PathValue(0, 0, matrix(0)(0), List(Point(0, 0, matrix(0)(0)))), initialPath)
    }
    
  }
  
  def main(args : Array[String]) : Unit = {
    val (path, time) =  evaluatePath(getMatrixFromFile, new LeftToRightDownPathCalculator)
    
    println("==================")
    println(path)
    println("Time = " + time + " ms")
  }
}