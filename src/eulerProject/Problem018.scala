package eulerProject

import scala.io.Source
import scala.io.Source._

/**
 * Problem 18: Find the maximum sum travelling from the top of the triangle 
 * to the base.<br/>
 * 31 May 2002<br/>
 * <br/>
 * By starting at the top of the triangle below and moving to adjacent numbers on 
 * the row below, the maximum total from top to bottom is 23.<br/>
 * <pre>
 * 3+
 * 7+ 4
 * 2  4+ 6
 * 8  5  9+ 3
 * </pre>
 * That is, 3 + 7 + 4 + 9 = 23.<br/>
 * <br/>
 * <b>Find the maximum total from top to bottom of the triangle below:</b>
 * <pre>
 * 75
 * 95 64
 * 17 47 82
 * 18 35 87 10
 * 20 04 82 47 65
 * 19 01 23 75 03 34
 * 88 02 77 73 07 63 67
 * 99 65 04 28 06 16 70 92
 * 41 41 26 56 83 40 80 70 33
 * 41 48 72 33 47 32 37 16 94 29
 * 53 71 44 65 25 43 91 52 97 51 14
 * 70 11 33 28 77 73 17 78 39 68 17 57
 * 91 71 52 38 17 14 91 43 58 50 27 29 48
 * 63 66 04 68 89 53 67 30 73 16 69 87 40 31
 * 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
 * </pre>
 * NOTE: As there are only 16384 routes, it is possible to solve this problem 
 * by trying every route. However, Problem 67, is the same challenge with a 
 * triangle containing one-hundred rows; it cannot be solved by brute force, 
 * and requires a clever method! ;o)<br/>
 * 
 */
object Problem018 {
  
  
  case class Cell(row: Int, column: Int, value: Int) {
    
    lazy val price: Double = 1.0 / value
    
    override def toString = "Cell(%02d, %02d, %02d)".format(this.row, this.column, this.value)
    
  }
  
  case class Path(cells: List[Cell]) {
    
    def price: Double = this.cells.foldLeft(0.0)(_ + _.price)
      //this.cells.foldLeft(0.0)((sum, cell) => sum + 1.0 / cell.value) 
      //1.0 / this.totalValue
    
    def next(cell: Cell) = Path(cell :: cells)
    
    lazy val totalValue = cells.foldLeft(0)(_ + _.value)
    
    override def toString = "Path(%.5f, %4d, %s)".format(this.price, totalValue, cells.reverse) 
    
  }
  
  case object ExpansivePath extends Path(Nil) {
    override def price = Math.MAX_DOUBLE
    override def toString = "Path(MAX_DBL, %4d, %s)".format(totalValue, cells.reverse) 
  }
  
/*
 * 3+
 * 7+ 4
 * 2  4+ 6
 * 8  5  9+ 3
 */
  val strTriangle = """
3
7 4
2 4 6
8 5 9 3
"""
  
  val strTriangle2 = """
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
"""
  
  val strTriangle3 = """
75
95 64
17 07 02
18 35 87 10
"""
  
  def sourceToCells(source: Source): List[List[Cell]] = {
    
    def lineToCells(line: String, positionRow: Int): List[Cell] = {
      val strValues = line.trim.split(' ')
      (0 until strValues.size).map(col => Cell(positionRow, col, strValues(col).toInt)).toList
    }
    
    val lines = source.getLines.toList
    (0 until lines.size).map(positionRow => lineToCells(lines(positionRow), positionRow)).toList
  }
  
  def getGreatestPath(triangle: List[List[Cell]]): Path = {
    
    def walk(row: Int, column: Int, priorPath: Path, cheapestPath: Path): Path = {
      val currentPath = priorPath.next(triangle(row)(column))
      
//      println("(%03d, %03d)".format(row, column))
//      println("\tCurrent  = " + currentPath)
//      println("\tCheapest = " + cheapestPath)
      
      if(currentPath.price > cheapestPath.price){
        cheapestPath
      } else if(row + 1 == triangle.size) {
        println(currentPath)
        currentPath
      } else {
        val pathCandidate = walk(row + 1, column, currentPath, cheapestPath)
        walk(row + 1, column + 1, currentPath, pathCandidate)
      }
    }
    
    walk(0, 0, new Path(Nil), ExpansivePath)
  }
  
  def main(args : Array[String]) : Unit = {
    val t0 = System.currentTimeMillis
    val triangle = sourceToCells(Source.fromString(strTriangle2.trim))
    val result = getGreatestPath(triangle)
    val deltaT = System.currentTimeMillis - t0
    
    println("==================")
    println(result)
    println(result.totalValue)
    println("Time = " + deltaT + " ms")
  }
}
