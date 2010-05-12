package eulerProject.solved


/**
Problem 22.<br>
19 July 2002<br>
<br>
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file 
containing over five-thousand first names, begin by sorting it into 
alphabetical order. Then working out the alphabetical value for each name, 
multiply this value by its alphabetical position in the list to obtain a name 
score.<br>
<br>
For example, when the list is sorted into alphabetical order, COLIN, which is 
worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would 
obtain a score of 938 × 53 = 49714.<br>
<br>
What is the total of all the name scores in the file?<br>
<br>
 * EULER: SOLVED
 */
object Problem022 {
  
  def getCharValue(ch: Char): Int = if(ch == '"') 0 else (ch - 'A' + 1)
  
  def getNameValue(name: String): Int = {
    
    def calculate(chars: List[Char]): Int = chars match {
      case Nil => 0
      case ch :: rest => getCharValue(ch) + calculate(rest)
    }
    
    calculate(name.toCharArray.toList)
  }
  
  def main(args : Array[String]) : Unit = {
    val source = scala.io.Source.fromFile("names.txt")
    val names = source.getLine(1).split(",").toList.sort((s1, s2) => s1.compareTo(s2) < 0)
    
    var sum = 0L
    
    (0 until names.length).foreach(i => {
        val name = names(i)
        val value = getNameValue(name)
        println(name + " - " + value + " - " + i)
        sum += (value * (i + 1))
    })
    
    println(sum)
  }
}
