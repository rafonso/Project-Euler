package eulerProject.solved

/**
 * @param weekDay 1 = Sunday, 2 = Monday, ..., 7 = Saturday
 * @param monthDay
 * @param month 1 = January, 2 = Februaty, ..., 12 = December
 * @param year
 * EULER: SOLVED
 */
case class Day(weekDay: Int, monthDay: Int, month: Int, year: Int) 

class DayIterator(initialDay: Day) extends BufferedIterator[Day] {
  private var currentDay = initialDay
  
  def head: Day = currentDay
  
  def hasNext: Boolean = true
  
  def next: Day = {
    
    def getNextDay(): Int = {
      if((currentDay.monthDay == 28) && (currentDay.month == 2) && (currentDay.year % 4 != 0)) {
        1
      } else if((currentDay.monthDay == 29) && (currentDay.month == 2) && (currentDay.year % 4 == 0)) {
        1
      } else if((currentDay.monthDay == 30) && 
                  ((currentDay.month == 4) || (currentDay.month == 6) || (currentDay.month == 9) || (currentDay.month == 11))) {
        1
      } else if((currentDay.monthDay == 31) && 
                  ((currentDay.month == 1) || (currentDay.month == 3) || (currentDay.month == 5) || (currentDay.month == 7) || (currentDay.month == 8) || (currentDay.month == 10) || (currentDay.month == 12))) {
        1
      } else {
        currentDay.monthDay + 1
      } 
    }
    
    val nextWeekDay = if(currentDay.weekDay == 7) 1 else currentDay.weekDay + 1
    val nextMonthDay = getNextDay()
    var changeYear = false
    val nextMonth: Int = if(nextMonthDay == 1) {
      if(this.currentDay.month == 12) {
        changeYear = true
        1
      } else {
        this.currentDay.month + 1
      }
    } else {
      this.currentDay.month
    }
    val nextYear = if(changeYear) this.currentDay.year + 1 else this.currentDay.year
    
    this.currentDay = new Day(nextWeekDay, nextMonthDay, nextMonth, nextYear )
    this.currentDay 
  }
}

object Problem019Scla {
  def main(args : Array[String]) : Unit = {
    val dayIterator = new DayIterator(new Day(2, 1, 1, 1901))
    
    var sundaysInFirst = 0
    while(dayIterator.head.year < 2001) {
      if((dayIterator.head.weekDay == 1) && (dayIterator.head.monthDay == 1)) {
        println(dayIterator.head)
        sundaysInFirst += 1
      }
      dayIterator.next
    }
    println(sundaysInFirst )
  }
}
