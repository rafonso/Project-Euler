package eulerProject

package sudoku {
  

  
  class Puzzle(private val matrix: Array[Array[Option[Byte]]]) {
    
    private val areaSeperator = "------|------|------"
    
    def apply(row: Int, column: Int): Option[Byte] = matrix(row)(column)
    
    override def toString = {
      
      def rowToString(row: Array[Option[Byte]]): String = {
        (0 until 9).foldLeft("")((str, col) => {
          val str1 = if(row(col).isDefined) row(col).get.toString else "0"
          val str2 = if((col == 2) || (col == 5)) "|" else " "
          str + str1 + str2
        })
      }
      
      (0 until 9).foldLeft("")((str, row) => {
        val strLine = rowToString(this.matrix(row))
        val str1 = if(row < 8) "\n" else ""
        str + strLine + str1
      })
    }
    
  }
  
}

object Sudoku {
  
  type SudokuCell = Option[Int]
  type SudokuSequence = Array[SudokuCell]
  type SudokuMatrix = Array[Array[SudokuSequence]]
  
  import scala.io.Source
  
  def parse(source: Source): SudokuMatrix = {
    
    def parseRow(row: String): SudokuSequence = {
      
      def charToCell(pos: Int): SudokuCell = row(pos) match {
        case '0' => None
        case '1' | '2' |'3' | '4' | '5' | '6' | '7' | '8' | '9' => Some(row(pos) - '0')
        case _ => error("Linha não Numérica: " + row)
      }
      
      if(row.size != 9) {
        error("line with more Than 9 numbers: " + row)
      }
      
      Array(charToCell(0), charToCell(1), charToCell(2), charToCell(3), 
            charToCell(4), charToCell(5), charToCell(6), charToCell(7), 
            charToCell(8))
    }
    
    val lines = source.getLines.toList
    if(lines.size != 9) {
      error("Sudoku With no 9 lines: " + lines)
    }
    
    null
    
    //lines.map(parseRow(_)).toArray
  }
  
}

