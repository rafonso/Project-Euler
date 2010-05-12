package eulerProject

import scala.io._

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
object Problem081a {
  
  type Matrix = List[List[Int]]
  
  case class Point(row: Int, col: Int, value: Int) {
    
    override def toString: String = "Point(%2d, %2d, %4d)".format(this.row, this.col, this.value)
    
  }
  
  case class PathValue(currentRow: Int, currentColumn: Int, value: Int, inversePath: List[Point]) {
    
    def next(row: Int, col: Int, nextValue: Int): PathValue = 
      PathValue(row, col, this.value + nextValue, Point(row, col, nextValue) :: this.inversePath)
    
    override def toString: String = "PathValue(%2d, %2d, %5d, %s)".format(this.currentRow, this.currentColumn, this.value, this.inversePath.reverse)
  } 
  
  def getMinimalPath(matrix: Matrix): PathValue = {
    
    val maxRow = matrix.size - 1
    val maxCol = matrix(0).size - 1
    
    def calculatePath(currentPath: PathValue, smallestPath: PathValue): PathValue = {
      if(currentPath.value < smallestPath.value) {
        if(currentPath.currentRow == maxRow && currentPath.currentColumn == maxCol) {
          val now = new java.util.Date
          println("[%tT.%tL] %s".format(now, now, currentPath))
          currentPath
        } else if(currentPath.currentRow == maxRow && currentPath.currentColumn <  maxCol) {
          calculatePath(currentPath.next(maxRow, currentPath.currentColumn + 1, matrix(maxRow)(currentPath.currentColumn + 1)), smallestPath)
        } else if(currentPath.currentRow <  maxRow && currentPath.currentColumn == maxCol) {
          calculatePath(currentPath.next(currentPath.currentRow + 1, maxCol, matrix(currentPath.currentRow + 1)(maxCol)), smallestPath)
        } else if(currentPath.currentRow <  maxRow && currentPath.currentColumn < maxCol) {
          val nextPath = calculatePath(currentPath.next(currentPath.currentRow + 1, currentPath.currentColumn, matrix(currentPath.currentRow + 1)(currentPath.currentColumn)), smallestPath)
          calculatePath(currentPath.next(currentPath.currentRow, currentPath.currentColumn + 1, matrix(currentPath.currentRow)(currentPath.currentColumn + 1)), nextPath)
        } else {
          error("Irregular Path: " + currentPath)
        }
      } else {
        smallestPath
      }
    }
    
    calculatePath(PathValue(0, 0, matrix(0)(0), List(Point(0, 0, matrix(0)(0)))), PathValue(0, 0 ,Math.MAX_INT, Nil))
  }
  
  def sourceToMatrix(source: Source): Matrix = {
    
    def lineToMatrixLine(line: String): List[Int] =
      line.trim.split(Array(' ', '\n', ',')).map(_.toInt).toList
    
    source.getLines.map(lineToMatrixLine(_)).toList
  }
  
  val strMatrix = """
131 673 234 103 18
201 96 342 965 150
630 803 746 422 111
537 699 497 121 956
805 732 524 37 331
"""
  
/*
131 673 234 103 18
201 96 342 965 150
630 803 746 422 111
537 699 497 121 956
805 732 524 37 331
*/  
  
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
/*
PathValue( 3928, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 2,  0,  630), Point( 3,  0,  537), Point( 4,  0,  805), Point( 4,  1,  732), Point( 4,  2,  524), Point( 4,  3,   37), Point( 4,  4,  331)))
PathValue( 3822, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 2,  0,  630), Point( 3,  0,  537), Point( 3,  1,  699), Point( 4,  1,  732), Point( 4,  2,  524), Point( 4,  3,   37), Point( 4,  4,  331)))
PathValue( 3587, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 2,  0,  630), Point( 3,  0,  537), Point( 3,  1,  699), Point( 3,  2,  497), Point( 4,  2,  524), Point( 4,  3,   37), Point( 4,  4,  331)))
PathValue( 3184, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 2,  0,  630), Point( 3,  0,  537), Point( 3,  1,  699), Point( 3,  2,  497), Point( 3,  3,  121), Point( 4,  3,   37), Point( 4,  4,  331)))
PathValue( 2916, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 1,  1,   96), Point( 2,  1,  803), Point( 3,  1,  699), Point( 3,  2,  497), Point( 3,  3,  121), Point( 4,  3,   37), Point( 4,  4,  331)))
PathValue( 2888, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 1,  1,   96), Point( 2,  1,  803), Point( 2,  2,  746), Point( 2,  3,  422), Point( 3,  3,  121), Point( 4,  3,   37), Point( 4,  4,  331)))
PathValue( 2502, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 1,  1,   96), Point( 1,  2,  342), Point( 2,  2,  746), Point( 3,  2,  497), Point( 3,  3,  121), Point( 4,  3,   37), Point( 4,  4,  331)))
PathValue( 2427, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 1,  1,   96), Point( 1,  2,  342), Point( 2,  2,  746), Point( 2,  3,  422), Point( 3,  3,  121), Point( 4,  3,   37), Point( 4,  4,  331)))
==================
PathValue( 2427, List(Point( 0,  0,  131), Point( 1,  0,  201), Point( 1,  1,   96), Point( 1,  2,  342), Point( 2,  2,  746), Point( 2,  3,  422), Point( 3,  3,  121), Point( 4,  3,   37), Point( 4,  4,  331)))
Time = 275 ms
*/
