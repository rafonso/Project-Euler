package eulerProject

/**
 * Problems 81, 82 and 83
 */
object RectangularPath {
  
  import scala.io.Source
  
  type Matrix = List[List[Int]]
  
  case class Point(row: Int, col: Int, value: Int) {
    
    override def toString: String = "Point(%2d, %2d, %4d)".format(this.row, this.col, this.value)
    
  }
  
  case class PathValue(currentRow: Int, currentColumn: Int, value: Int, inversePath: List[Point]) {
    
    def next(row: Int, col: Int, nextValue: Int): PathValue = 
      PathValue(row, col, this.value + nextValue, Point(row, col, nextValue) :: this.inversePath)
    
    def containsPonit(row: Int, column: Int) = inversePath.exists(point => (point.row == row) && (point.col == column))
    
    override def toString: String = "PathValue(%2d, %2d, %5d, %s)"
      .format(this.currentRow, this.currentColumn, this.value, this.inversePath.reverse)
  }
  
  trait PathCalculator {
    
    def showPath(path: PathValue) {
      val now = new java.util.Date
      println("[%tT.%tL] %s".format(now, now, path))
    }
    
    def getMinimalPath(matrix: Matrix): PathValue 
    
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
""".trim
  
  def getMatrixFromStrMatrix: Matrix = sourceToMatrix(Source.fromString(strMatrix.trim))
  
  def getMatrixFromFile     : Matrix = sourceToMatrix(Source.fromFile("matrix.txt"  ))
  
  def evaluatePath(matrix: Matrix, pathCalculator: PathCalculator): (PathValue, Long) = {
    val t0 = System.currentTimeMillis
    val result = pathCalculator.getMinimalPath(matrix)
    val deltaT = System.currentTimeMillis - t0
    
    (result, deltaT)
  }
  
  val initialPath = PathValue(-1, -1, Math.MAX_INT, Nil)
  
}
