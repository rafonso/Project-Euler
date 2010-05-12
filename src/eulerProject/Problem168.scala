package eulerProject

/**
 * Problem 168: Number Rotations<br>
 * 16 November 2007<br>
 * <br>
 * Consider the number 142857. We can right-rotate this number by moving the 
 * last digit (7) to the front of it, giving us 714285.<br>
 * It can be verified that 714285=5×142857.<br>
 * This demonstrates an unusual property of 142857: it is a divisor of its 
 * right-rotation.<br>
 * <br>
 * <b>Find the last 5 digits of the sum of all integers n, 10 < n < 10^(100), 
 * that have this property.</b><br>
 * <br>
 */
object Problem168 {
  
  def isRotationMultiple(n: BigInt, pow10: BigInt): Boolean = {
    val lastDigit = n % 10
    val rotation = lastDigit * pow10 + n / 10
    rotation % n == 0
  }
  
  val digits = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  def mkPermutations(size: Int): Seq[BigInt] = {
    
    if(size == 0) {
      return Nil
    }
    
    def calculate(disponibleDigits: List[Int], currentNumber: BigInt, accumulatedNumbers: List[BigInt]): Seq[BigInt] = {
      disponibleDigits match {
        case Nil => accumulatedNumbers
        case List(d) => (currentNumber * 10 + d) :: accumulatedNumbers
        case _ => {
          (0 to disponibleDigits.size).flatMap(i => {
            val splittedDigits = disponibleDigits.splitAt(i)
            val remainedDigits = splittedDigits ._1 ::: splittedDigits._2
            val digit = disponibleDigits(i)
            val nextNumber = currentNumber * 10 + digit
            calculate(remainedDigits, nextNumber, accumulatedNumbers)
          })
        }
      }
    }
    
    calculate(digits, BigInt(0), Nil)
  }
  
  def main(args : Array[String]) : Unit = {
    /*
    val combinacoes = for(i <- 1 to 9; j <- i + 1 to 9) yield (i, j)
    println(combinacoes)
    println(isRotationMultiple(142852, 100000))
    */
    println(mkPermutations(1))
  }
}

/*
type Type = Char
val C0: Char = 0

def show(i: Int, arrays: List[Array[Type]]) = {
    print(i + " => ")
    arrays.foreach(a => print(a.toString + " "))
    println
}

def multiply(elem: Type, arrOriginal: Array[Type]): List[Array[Type]] = {
   
    def generate(pos: Int, arrays: List[Array[Type]]): List[Array[Type]] = {
        if(pos == arrOriginal.size) arrays.reverse
        else {
            val copy = new Array[Type](arrOriginal.size)
            Array.copy(arrOriginal, 0, copy, 0, arrOriginal.size)
            if(copy(pos) == 0) {
                copy(pos) = elem
                println(pos + " => " + copy.toString)
                generate(pos + 1, copy :: arrays)
            }
            else generate(pos + 1, arrays)
        }
    }

    generate(0, Nil)
}

def getPermutations(originals: List[Type]): List[Array[Type]] = {

	def generate(elems: List[Type], permutations: List[Array[Type]]): List[Array[Type]] = elems match {
		case Nil => permutations
		case el :: others => generate(others, permutations.flatMap(multiply(el, _)))
	}

	generate(originals, Nil)
}

val originals = List('0', '1', '2')
val result = getPermutations(originals)
result.foreach(a => print(a.toString + " "))
println
println("----------------------------------------------------")
/*
val val0: Type = 0
val arr1 = Array.make(3, val0)
val copies = multiply('0', arr1)
copies.foreach(a => print(a.toString + " "))
println
val copies2 = copies.flatMap(a => multiply('1', a))
println("----------------------------------------------------")
copies2.foreach(a => print(a.toString + " "))
println
val copies3 = copies2.flatMap(a => multiply('2', a))
println("----------------------------------------------------")
copies3.foreach(a => print(a.toString + " "))
println
println("=====================================================")
*/ * 
================================================================================
 * 
type MyArray = Array[Char]


def getPermutations(array: MyArray): List[MyArray] = {

	def generateFirstArrays(index: Int, listOfArrays: List[MyArray]): List[MyArray] = {
		if(index < array.size) {
			val arr = new MyArray(array.size)
			arr(index) = array(0)
			generateFirstArrays(index + 1, arr :: listOfArrays)
		} else {
			listOfArrays.reverse
		}
	}

	def multiply(el: Char, arr: MyArray): List[MyArray] = {
		
	}

	def generate(elems: List[Char], permutations: List[Array[Char]]): List[Array[Char]] = elems match {
		case Nil => permutations
		case el :: others => {
			
		}
	}

	val a1 = generateFirstArrays(0, Nil)
	a1
}


val arr = Array('1', '2', '3')
val result = getPermutations(arr)
result.foreach(a => print(a.toString + ", "))
println()

 */
