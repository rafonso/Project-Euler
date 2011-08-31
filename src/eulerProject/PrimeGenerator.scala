package projectEuler

import scala.annotation.tailrec

/**
 * Gerador de n�meros primos. Fornece m�todos para a obten��o dos n primeiros n�meros primos, para a obten��o dos primos
 * menores ou igual a determinado valor e finalmente para verificar se determinado n�mero � realmente primo.
 *
 */
class PrimesGenerator {

  private def errorMessage(n: Long) = "Number should be positive but is " + n

  private def isNumberPrime(n: Long, previousPrimes: Iterable[Long]): Boolean = {
    val limit = scala.math.sqrt(n).toLong
    previousPrimes.takeWhile(_ <= limit).forall(n % _ != 0)
  }

  @tailrec
  private def nextPrime(n: Long, previousPrimes: Iterable[Long]): Long = {
//    println("\tnextPrime(%d, %s)".format(n, previousPrimes))
    if (isNumberPrime(n, previousPrimes)) n
    else nextPrime(n + 2, previousPrimes)
  }

  /**
   * Stream dos n�meros primos. Come�a com 2 depois 3 e da� em diante vai experimentando os n�meros �mpares
   * experimentando uma a um comparando-os com os primos anteriores.
   */
  private val primes: Stream[Long] = Stream.cons(2L, Stream.iterate(3L)(n => nextPrime(n + 2, primes)))

  /**
   * Retorna os n primeiros n�meros primos.
   *
   * @exception IllegalArgumentException Se n for negativo.
   */
  def take(n: Int): Seq[Long] = {
    require(n > 0, errorMessage(n))

    this.primes.take(n)
  }

  /**
   * Retorna os n�meros primos at� um determinado n�mero (inclusive).
   *
   * @param n Limite da sequencia de primos
   * @return primos menores ou igual a n
   * @exception IllegalArgumentException Se n for negativo.
   */
  def takeUntil(n: Long): Seq[Long] = {
    require(n > 0, errorMessage(n))

    this.primes.takeWhile(_ <= n)
  }

  /**
   * Verifica se determinado � primo.
   * @param n N�mero a ser avaliado
   * @return <code>true</code> se � Primo, <code>false</code> caso contr�rio.
   * @exception IllegalArgumentException Se n for negativo.
   */
  def isPrime(n: Long): Boolean = n match {
    case x if (x < 0)      => throw new IllegalArgumentException(errorMessage(x))
    case 0 | 1             => false
    case 2                 => true
    case x if (x % 2 == 0) => false
    case _                 => this.isNumberPrime(n, this.primes)
  }
  
  def main(args: Array[String]): Unit = {
    println(takeUntil(100).toList)
  }

}