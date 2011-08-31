package projectEuler

import scala.annotation.tailrec

/**
 * Gerador de números primos. Fornece métodos para a obtenção dos n primeiros números primos, para a obtenção dos primos
 * menores ou igual a determinado valor e finalmente para verificar se determinado número é realmente primo.
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
   * Stream dos números primos. Começa com 2 depois 3 e daí em diante vai experimentando os números ímpares
   * experimentando uma a um comparando-os com os primos anteriores.
   */
  private val primes: Stream[Long] = Stream.cons(2L, Stream.iterate(3L)(n => nextPrime(n + 2, primes)))

  /**
   * Retorna os n primeiros números primos.
   *
   * @exception IllegalArgumentException Se n for negativo.
   */
  def take(n: Int): Seq[Long] = {
    require(n > 0, errorMessage(n))

    this.primes.take(n)
  }

  /**
   * Retorna os números primos até um determinado número (inclusive).
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
   * Verifica se determinado é primo.
   * @param n Número a ser avaliado
   * @return <code>true</code> se é Primo, <code>false</code> caso contrário.
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