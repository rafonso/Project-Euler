package eulerProject

/**
 * Problem 258: A lagged Fibonacci sequence<br>
 * 03 October 2009<br>
 * <br>
 * A sequence is defined as:<br>
 * <br>
 * g<sub>k</sub> = 1, for 0 <= k <= 1999<br>
 * g<sub>k</sub> = g<sub>k - 2000</sub> + g<sub>k - 1999</sub>, for k >= 2000.<br>
 * 
 * <br>
 * <b>Find g<sub>k</sub> mod 20092010 for k = 10<sup>18</sup>.</b><br> 
 */
object Problem258 {
  
  private class FibonacciIterator extends BufferedIterator[BigInt] {
    
    private val ONE = BigInt(1)
    
    private var index = 0L
    
    private var current = ONE
    
    private val queue = new scala.collection.mutable.Queue[BigInt]
    (0 until 2000).foreach((i) => queue += ONE)
    
    private[this] var event: Option[(Long, BigInt) => Unit] = None
    
    def setEvent(evt: Option[(Long, BigInt) => Unit]): Unit = this.event = evt
    
    def currentIndex = this.index
    
    def head = this.current
    
    def hasNext = true
    
    def next = {
      if(index < 2000) {
        index += 1
        ONE
      } else {
        this.current = this.queue(0) + this.queue(1)
        this.queue.dequeue
        this.queue.enqueue(this.current)
        if(event.isDefined) event.get.apply(this.index, this.current)
        index += 1
        this.current
      }
    }
    
  }
  
  def showIndex(index: Long, value: BigInt)= {
    val div = 20092010 
    if(index % 100000 == 0) {
      printf("%,25d => %,010d%n", index, (value % div).longValue)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val max = 100000000 //(10e9).longValue

    val t0 = System.currentTimeMillis
    val it = new FibonacciIterator 
    it.setEvent(Some(showIndex))    
    while(it.currentIndex <= max) {
      it.next
    }
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println("Total Time: " + deltaT + " ms")
  }
}
