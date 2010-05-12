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
  
  def main(args : Array[String]) : Unit = {
    println(getPrimesUntil(100))
  }

}
