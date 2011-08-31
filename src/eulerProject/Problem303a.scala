package eulerProject

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
object Problem303a extends App {

    import Utils._
    
    val generation1 : Stream[Long] = Stream(1L, 2L)

    def getNextGeneration(generation : List[Long]) : Stream[Long] = { // (numberStream: Stream[Long]) 
        log("\tgetNextGeneration gerando para : %,d".format(generation.size))
        val nextGeneration = generation.flatMap(n => {
            val base = n * 10
            Array(base, base + 1, base + 2)
        })
        log("\tgetNextGeneration gerado : %,d -> LAST: %,d".format(nextGeneration.size, nextGeneration.last))
        
        nextGeneration.toStream.append(getNextGeneration(nextGeneration))
    }

    val validNumbers : Stream[Long] = generation1.append(getNextGeneration(generation1.toList))

    def f(n : Long) : Long = validNumbers.dropWhile(_ < n).find(_ % n == 0).get

    val max = args(0).toInt

    (1 to max).foreach(n => log("f(%4d) = %,20d".format(n, f(n))))

}

/*
D:\workspaces\workspace-euler\Project-Euler\src>scalac eulerProject\Problem303a.scala
D:\workspaces\workspace-euler\Project-Euler\src>scala eulerProject.Problem303a 10
*/