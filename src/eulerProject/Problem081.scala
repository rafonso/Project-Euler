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
object Problem081 {
  
  /*
  def getSum(matrix: Array[Array[Int]], row: Int, column: Int, sum: Int): Int = {
    val lastRow    = (row    == (matrix.length - 1))
    val lastColumn = (column == (matrix(0).length - 1))
    
    if(lastRow && lastColumn) sum
    else if(lastRow)          getSum(matrix, row,     column + 1, matrix(row)(column + 1) + sum)
    else if(lastColumn)       getSum(matrix, row + 1, column    , matrix(row + 1)(column) + sum)
    else {
      val nextRowValue =    matrix(row + 1)(column)
      val nextColumnValue = matrix(row)(column + 1)
      if(nextRowValue < nextColumnValue) getSum(matrix, row + 1, column    , nextRowValue    + sum)
      else                               getSum(matrix, row    , column + 1, nextColumnValue + sum)
    }
  }
  */
  type Matrix = List[List[Int]]
  
  case class Point(row: Int, col: Int)
  
  case class PathValue(value: Int, inversePath: List[Point]) {
    
    def next(row: Int, col: Int, nextValue: Int): PathValue = 
      PathValue(this.value + nextValue, Point(row, col) :: this.inversePath)
    
  } 
  
  def getMinimalPath(matrix: Matrix): PathValue = {
    
    val maxRow = matrix.size - 1
    val maxCol = matrix(0).size - 1
    
    def calculatePath(currentPath: PathValue, smallestPath: PathValue): PathValue = {
      val currPoint = currentPath.inversePath.first
      if(currPoint.row == maxRow && currPoint.col == maxCol) {
        println("-----------------------------")
        println("Current : " + currentPath)
        println("Smallest: " + smallestPath)
        if(currentPath.value < smallestPath.value) currentPath
        else smallestPath
      } else if(currPoint.row == maxRow && currPoint.col < maxCol) {
        if(currentPath.value < smallestPath.value) calculatePath(currentPath.next(maxRow, currPoint.col + 1, matrix(maxRow)(currPoint.col + 1)), smallestPath)
        else smallestPath
      } else if(currPoint.row < maxRow && currPoint.col == maxCol) {
        if(currentPath.value < smallestPath.value) calculatePath(currentPath.next(currPoint.row + 1, maxCol, matrix(currPoint.row + 1)(maxCol)), smallestPath)
        else smallestPath
      } else if(currPoint.row < maxRow && currPoint.col < maxCol) {
        val nextSmallestPath = calculatePath(currentPath.next(currPoint.row + 1, currPoint.col, matrix(currPoint.row + 1)(currPoint.col)), smallestPath)
        calculatePath(currentPath.next(currPoint.row, currPoint.col + 1, matrix(currPoint.row)(currPoint.col + 1)), nextSmallestPath)
      } else {
        error("Irregular Path: " + currentPath)
      }
    } 
    
    calculatePath(PathValue(matrix(0)(0), List(Point(0, 0))), PathValue(Math.MAX_INT, Nil))
  }
  
  
  val strMatrix = """
131 673 234 103 18
201 96 342 965 150
630 803 746 422 111
537 699 497 121 956
805 732 524 37 331
"""
  
  import scala.io._

  def sourceToMatrix(source: Source): Matrix = {
    
    def lineToMatrixLine(line: String): List[Int] =
      line.trim.split(Array(',', '\n')).map(_.toInt).toList
    
    source.getLines.map(lineToMatrixLine(_)).toList
  }

  def main(args : Array[String]) : Unit = {
    
    val t0 = System.currentTimeMillis
//    val source = Source.fromString(strMatrix.trim)
    val source = Source.fromFile("matrix.txt")
    val matrix = sourceToMatrix(source)    
    val result = getMinimalPath(matrix)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println("Time = " + deltaT + " ms")
  }
}
