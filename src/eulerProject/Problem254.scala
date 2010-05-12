package eulerProject

/**
 * Problem 254: Sums of Digit Factorials.<br>
 * 04 September 2009<br>
 * <br>
 * Define f(n) as the sum of the factorials of the digits of n. 
 * For example, f(342) = 3! + 4! + 2! = 32.<br>
 * <br>
 * Define sf(n) as the sum of the digits of f(n). So sf(342) = 3 + 2 = 5.<br>
 * <br>
 * Define g(i) to be the smallest positive integer n such that sf(n) = i. Though 
 * sf(342) is 5, sf(25) is also 5, and it can be verified that g(5) is 25.<br>
 * <br>
 * Define sg(i) as the sum of the digits of g(i). So sg(5) = 2 + 5 = 7.<br>
 * <br>
 * Further, it can be verified that g(20) is 267 and sum(sg(i)) for 1 <= i <= 20 is 156.<br>
 * <br>
 * <b>What is sum(sg(i)) for 1 <= i <= 150?<b><br>
 * 
 * PROBLEMAS: Até sg(40) o cálculo dura menos de um segundo. Entreanto a partir 
 * daí o tempo de processamento explode. Para 45, o tempo foi de 17 s. Para 46 o 
 * tempo foi de 212 s. Para 47 interrompi depois de 20 minutos. 
 */
object Problem254 {
  
  val factorialDigits = Array(0, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
  
  def sumDigits(n: Long, f: Long => Long): Long = {
    
    def calculate(number: Long, lastDigit: Long, sum: Long): Long = 
      if(number == 0) sum + f(lastDigit) 
      else calculate(number / 10, (number % 10).toLong, f(lastDigit) + sum)
    
    calculate(n / 10, (n % 10).toLong, 0)
  }
  
  def f(n: Long): Long = sumDigits(n, (i: Long) => factorialDigits(i.toInt))
  
  def sf(n: Long): Long = sumDigits(f(n), (i: Long) => i)
  
  def g(n: Long): Long = {
    
    def calculate(i: Long): Long = 
      if(sf(i) == n) i 
      else calculate(i + 1)
    
    calculate(0)
  }
  
  def sg(n: Int): Int = {
    val t0 = System.currentTimeMillis
    val sum = sumDigits(g(n), (i: Long) => i)
    val deltaT = System.currentTimeMillis - t0
    
    printf("sg(%3d) = %4d (time = %,d ms)%n", n, sum, deltaT)
    
    sum.toInt
  }
  
  def main(args : Array[String]) : Unit = {
    val sum = (1 to 46).foldLeft(0)((acc, i) => acc + sg(i))
    println("=================================================")
    println("SUM = " + sum)
  }
}
