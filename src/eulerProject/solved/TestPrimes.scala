package eulerProject.solved

object TestPrimes {
  
  def calculate(times: Int, f: (Long => List[Long]), title: String): Long = {
    printf("%,12d ", times)
    val t0 = System.nanoTime
    val primes = f(times)
    val deltaT = System.nanoTime - t0
    printf("%,15d %,15d%n", deltaT, primes.size)
    deltaT
  }
  
  def getPrimesUntilEratosthenes(n: Long): List[Long] = {
    def ints(x: Int): Stream[Long] = Stream.cons(x, ints(x + 1))
    
    def primes(nums: Stream[Long]): Stream[Long] = Stream.cons(nums.head, primes((nums.tail).filter(x => x % nums.head != 0)) )

    primes(ints(2)).take(n.toInt).toList
  }
  
  def getPrimesUntilBoolean(n : Long): List[Long] = {
    if(n < 2) {
      return Nil
    }
    if(n == 2) {
      return 2 :: Nil
    }
    
    val booleans = new Array[Boolean](n.toInt + 1)
    val primes = new scala.collection.mutable.ListBuffer[Long]
    
    // Elimnitate even numbers
    booleans(2) = true
    (2 until booleans.length).foreach(i => if(i % 2 == 0) booleans(i) = true)
    primes + 2
    
    (2 until booleans.length).foreach(i => {
      if(!booleans(i)) {
        (i until booleans.length by i).foreach(j => booleans(j) = true)
        primes + i
      }
    })
    
    primes.toList
  }

  def main(args : Array[String]) : Unit = {
    val limits = Array(1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000, 1000000, 3000000, 10000000, 30000000, 100000000, 300000000) //, 30000000)
    val eratosthenesTimes = new Array[Long](limits.length)
    val booleanTimes = new Array[Long](limits.length)
    
    eulerProject.Utils.getPrimesUntil(10L)
    getPrimesUntilBoolean(10)
    
    var i = 0
    limits.foreach(limit => {
      //myTimes(i) =           calculate(limits(i), PrimesUtils.getPrimesUntil, "getPrimesUntil")
      booleanTimes(i) = calculate(limits(i), eulerProject.Utils.getPrimesUntil, "booleans")
      i += 1
    })
    
    booleanTimes.foreach(println(_))
  }
}
