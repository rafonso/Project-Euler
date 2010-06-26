package eulerProject

object Utils {

  private def getPrimesFromFile(n: Long): List[Long] = {
    import scala.io._
    
    def generatePrimes(itStrPrimes: Iterator[String], primes: List[Long]): List[Long] = {
      val currentPrime = itStrPrimes.next.trim.toLong
      if(currentPrime > n) primes.reverse
      else if(itStrPrimes.hasNext) generatePrimes(itStrPrimes, currentPrime :: primes)
      else (currentPrime :: primes).reverse
    }
    
    val fileName = "primes.txt"
    val sourcePrimes = Source.fromFile(fileName)
    generatePrimes(sourcePrimes.getLines, Nil)
  }
  
  /**
   * Retorna os primos menores que um determinado número n.  
   * O maior primo disponível atualmente é um pouco maior que 50.000.000 
   */
  def getPrimesUntil(n: Long): List[Long] = {
    try {
      getPrimesFromFile(n)
    } catch {
      case e: java.lang.OutOfMemoryError => throw new java.lang.OutOfMemoryError("I need more Memory! Try '-Xmx1024m'") 
    }
  }
  
  def getPrimesUntil: List[Long] = getPrimesUntil(Math.MAX_LONG)
  
  def getPrimesUntil(n: Int): List[Long] = getPrimesUntil(n.toLong)

  /**
   * REtorna o Raiz inteira de um número inteiro ou então a raiz em ponto flutuante.
   * 
   * @param n Número a ter sua raiz avaliada
   * @return Raiz inteira de n (Right) ou a raiz em ponto flutuante (Leftt)
   */
  def getSqrt(n: Long): Either[Double, Long] = {
    val sqrtDbl = Math.sqrt(n)
    val sqrtLng = sqrtDbl.toLong
    if(sqrtLng * sqrtLng == n) Right(sqrtLng)
    else Left(sqrtDbl)
  }
  
  def getSqrt(n: BigInt): Either[Double, BigInt] = {
    
    def evaluate(rootAnt: BigInt, root: BigInt): BigInt = {
      if(rootAnt <= root) root
      else evaluate(root, (root + n / root) / 2)
    }
    
    val root = evaluate(n * 2, n)
    if(root * root == n) Right(root)
    else Left(Math.sqrt(n.doubleValue))
  }
  
  def /%(num: Int , div: Int ): (Int , Int ) = (num / div, num % div)
  
  def /%(num: Long, div: Long): (Long, Long) = (num / div, num % div)
  
  def log(msg: Any) {
    val now = new java.util.Date
    println("[%tT.%tL] %s".format(now, now, msg))
  }
  
  /**
   * Classe que retorna o quociente e o resto da divisão de um Número Inteiro.
   */
  class IntDivMod(dividend: Int) {
    
    /**
     * Retorna o quociente e o resto da divisão de dois {@link Int Inteiro}s
     * 
     * @param divisor Divisor {@link Int Inteiro}
     * @return O quociente e o resto da divisão como dois {@link Int Inteiro}s.
     */
    def /% (divisor: Int):  (Int, Int)   = (this.dividend / divisor, this.dividend % divisor)
    
    /**
     * Retorna o quociente e o resto da divisão de um {@link Int Inteiro} por um {@link Long}
     * 
     * @param divisor Divisor {@link Long}
     * @return O quociente e o resto da divisão como dois {@link Long}s.
     */
    def /% (divisor: Long): (Long, Long) = (this.dividend / divisor, this.dividend % divisor)
    
  } 
  
  /**
   * Classe que retorna o quociente e o resto da divisão de um Long.
   */
  class LongDivMod(dividend: Long) {
    
    /**
     * Retorna o quociente e o resto da divisão de dois {@link Long}s
     * 
     * @param divisor Divisor {@link Long}
     * @return O quociente e o resto da divisão como dois {@link Long}s.
     */
    def /% (divisor: Long): (Long, Long) = (this.dividend / divisor, this.dividend % divisor)
    
    /**
     * Retorna o quociente e o resto da divisão de um {@link Long} por um {@link Int Inteiro}
     * 
     * @param divisor Divisor {@link Int Inteiro}
     * @return O quociente e o resto da divisão como dois {@link Long}s.
     */
    def /% (divisor: Int):  (Long, Long) = (this.dividend / divisor, this.dividend % divisor)
    
  }
  
  implicit def intToDivMod(dividend: Int)   = new IntDivMod (dividend)
  
  implicit def longToDivMod(dividend: Long) = new LongDivMod(dividend)
  
  def intToBoolList(n: Int, size: Int): List[Boolean] = {
    
    def fillZeros(strBin: String): String = {
      if(strBin.size < size) fillZeros("0" + strBin)
      else if(strBin.size == size) strBin
      else error("Conversao de " + n + " para binário gerou '" + strBin 
                 + "' de tamanho " + strBin.size 
                 + " quando o tamanho máximo deve ser " + size)
    }
    
    fillZeros(n.toBinaryString).map(ch => if(ch == '1') true else false).toList
  } 
    
  
  def main(args : Array[String]) : Unit = {
//    println(numberToBinString(3624, 13))
    println(intToBoolList(3624, 13))
  }

}
