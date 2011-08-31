package projectEuler

import scala.collection.immutable._

/**
 * Problem 303<br/>
 * 25 September 2010<br/>
 * <br/>
 * For a positive integer n, define f(n) as the least positive multiple of n that, written in base 10, uses only digits <= 2.
 * <br/>
 * Thus f(2)=2, f(3)=12, f(7)=21, f(42)=210, f(89)=1121222.<br/>
 * <br/>
 * Also, <img src="http://projecteuler.net/project/images/p303_formula100.gif" style="vertical-align: middle;" alt="sum(1, 100, f(n)/n) = 11.363.107" />.<br/>
 * <br/>
 * <b>Find <img src="http://projecteuler.net/project/images/p303_formula10000.gif" style="vertical-align: middle;" alt="sum(1, 10000)"/>.</b>
 */
object Problema303 {

  type Number = Long
  val ONE: Number = 1
  //    BigInt(1)
  val TWO: Number = 2
  //    BigInt(2)
  val TEN: Number = 10
  //BigInt(10)

  def isValid(n: Number): Boolean = {
    if (n == 0) true
    else if (n % TEN > TWO) false
    else isValid(n / TEN)
  }

  def nextNumber(n: Number): Number = {
    require(n < Long.MaxValue - 10)
    if (isValid(n)) n else nextNumber(n + ONE)
  }

  val sequence: Stream[Number] = Stream.iterate(ONE)(n => nextNumber(n + ONE))

  def f(n: Number): Number = sequence.dropWhile(_ < n).find(_ % n == 0).get

  def main(args: Array[String]): Unit = {

    val t0 = System.currentTimeMillis
    (1 to 1000).foreach(n => println("f(%4d) = %,20d".format(n, f(n))))
    val deltaT = System.currentTimeMillis - t0

    println("time: %,d ms".format(deltaT))
  }
}
