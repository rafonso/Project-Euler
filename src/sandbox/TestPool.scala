package sandbox

import java.util.concurrent._
import scala.actors._

object TestPool {
  
  def eval(n: Int): Boolean = (n % 3 == 0) || (n % 5 == 0)
  
  def closePool(pool: ExecutorService) {
    pool.shutdown
    pool.awaitTermination(Math.MAX_LONG, TimeUnit.SECONDS)
  }
  
  def runSingle(max: Int): Long = (1 until max).filter(eval(_)).foldLeft(0L)(_ + _)
  
  def runPool(max: Int): Long = {
    
    def getCallable(i: Int): Callable[Boolean] = new Callable[Boolean] { def call = eval(i) }
    
    val pool = Executors.newCachedThreadPool 
    val result = (1 until max).filter(i => pool.submit(getCallable(i)).get).foldLeft(0L)(_ + _)
    closePool(pool)
    
    result
  }
  
  def runPoolJava(max: Int): Long = {
    var sum = 0L
    
    class SumCallable(n: Int) extends Callable[Int] {
      def call = {
        if(eval(n)) sum += n
        n
      }
    }
    
    val callables: java.util.List[Callable[Int]] = new java.util.ArrayList[Callable[Int]](max + 1)
    (1 until max).foreach(i => callables.add(new SumCallable(i)))
    
    val pool = Executors.newCachedThreadPool 
    pool.invokeAll(callables)
    closePool(pool)
    
    sum
  }
  
  def runActors(max: Int): Long = {
    import scala.actors.Actor._
    
    case class Add(n: Int)
    
    case object Result
    
    val act = actor {
      var sum = 0L
      loop {
        react {
          case Add(n) => if (eval(n)) { sum += n }
          case Result => reply(sum); exit
        }
      }
    }
   
    for (i <- 1 until max) act ! Add(i)
   
    act !? Result match {
      case result => result.toString.toLong
    }
  }

  
  /**
   * f é a função a ser executada. O retorno é uma Tuple2 contendo a soma e o 
   * tempo de execução.
   */
  def test(max: Int, f: Int => Long): (Long, Long) = {
    val t0 = System.currentTimeMillis
    val result = f(max)
    val deltaT = System.currentTimeMillis - t0
    
    (result, deltaT)
  }
  
  def testMax(max: Int) {
    println("* Max = " + max)
    println("Single  : " + test(max, runSingle))
    println("Pool    : " + test(max, runPool))
    println("PoolJava: " + test(max, runPoolJava))
    println("Actors  : " + test(max, runActors))
    println
  }
  
  def main(args : Array[String]) : Unit = {
    Array(0, 1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000).foreach(testMax(_))
  }
}
