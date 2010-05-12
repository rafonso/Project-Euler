package eulerProject

/**
 * Find the number of non-empty subsets of {1^(1), 2^(2), 3^(3),..., 
 * 250250^(250250)}, the sum of whose elements is divisible by 250. 
 * Enter the rightmost 16 digits as your answer.
 */
object Problem250Scala {
  
  val alpha = BigInt(10).pow(16).longValue
 
  
  def getSum(n: Int, divisor: Int, limit: Int): BigInt = {
    if(n % divisor == 0) println(n) 
    if(n >= limit) {
      BigInt(0)
    } else {
        val power = BigInt(n).pow(n) % alpha
        if((power % divisor) == 0) {
          println(power)
          power + getSum(n + 1, divisor, limit)
        } else {
          getSum(n + 1, divisor, limit)
        }
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val limit = Integer.parseInt(args(0))
    val divisor = Integer.parseInt(args(1))
    
     val t0 = System.currentTimeMillis
     val sum = getSum(1, divisor, limit) 
     val deltaT = System.currentTimeMillis - t0
     
     println("SUM = " + sum)
     println("Time = " + deltaT + " ns")
  }
}
/*
17811398107286805220585243431227223308190829043013304904123618730756798426479985931207282714051625014231040163989018308310957828797010
92857187079290079367379668681547789256550533690729459078468536750004939486648716080206378499786879933010468486984753805835229998925648862968
25352257784220210106069055994490424599833122856483696192362654307964969016425419635142130304445750418370613705415712672393765445735831561950
97942337216730573703668803831551174979495379896302904542173541586879392431004118637997607984251318602922708791747267651132094649000104857600
00
0000010000000000
 */