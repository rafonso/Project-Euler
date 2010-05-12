package eulerProject.solved

import scala.collection.mutable._

/**
 * Problem 62: Find the smallest cube for which exactly five permutations of 
 * its digits are cube.<br>
 * 30 January 2004<br>
 * <br>
 * The cube, 41063625 (345^(3)), can be permuted to produce two other cubes: 
 * 56623104 (384^(3)) and 66430125 (405^(3)). In fact, 41063625 is the smallest 
 * cube which has exactly three permutations of its digits which are also cube.<br>
 * <br>
 * <b>Find the smallest cube for which exactly five permutations of its digits 
 * are cube.<b><br>
 */
object Problem062a {
  
  def getCubesPermutation(max: Int): Set[Long] = {
    
    def evalN(n: Long, size: Int, permutations: MultiMap[Long, Long]): Set[Long] = {
      val cube = n * n * n
      val cubeStr = cube.toString
      val cubeSize = cubeStr.size
//      println("%5d^3 = %,15d".format(n, cube))
      if(cubeSize == size) {
        val base = cubeStr.toList.sort(_ < _).mkString.toLong
        permutations.add(base, cube)
        evalN(n + 1, size, permutations)
      } else {
        val result = permutations.find(_._2.size == max)
        if(result.isDefined) result.get._2
        else {
//          println("Cleaning Permutations")
          permutations.clear
          val base = cubeStr.toList.sort(_ < _).mkString.toLong
          permutations.add(base, cube)
          evalN(n + 1, cubeSize, permutations)
        }
      }
    }
    
    evalN(1, 1, new LinkedHashMap[Long, Set[Long]] with MultiMap[Long, Long])
  }
  
  def main(args : Array[String]) : Unit = {
    val maxPermutatios = 6
    
    val t0 = System.currentTimeMillis
    val result = getCubesPermutation(maxPermutatios)
    val deltaT = System.currentTimeMillis - t0
    
    println("==============================")
    println(result)
    println("Total Time: " + deltaT + " ms")
  }
}
