package eulerProject;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;

/**
 * Problem 104: Finding Fibonacci numbers for which the first and last nine
 * digits are pandigital.<br/>
 * 09 September 2005<br/>
 * <br/>
 * The Fibonacci sequence is defined by the recurrence relation:<br/>
 * <br/>
 * F(n) = F(n-1) + F(n-2), where F(1) = 1 and F(2) = 1.<br/>
 * <br/>
 * It turns out that F_(541), which contains 113 digits, is the first Fibonacci
 * number for which the last nine digits are 1-9 pandigital (contain all the
 * digits 1 to 9, but not necessarily in order). And F_(2749), which contains
 * 575 digits, is the first Fibonacci number for which the first nine digits are
 * 1-9 pandigital.<br/>
 * <br/>
 * <b>Given that F_(k) is the first Fibonacci number for which the first nine
 * digits AND the last nine digits are 1-9 pandigital, find k.</b><br/>
 * <br/>
 */
public class Problem104Java {

	private boolean isPandigital(String strNum) {
		Set<Character> digitsSet = new HashSet<Character>();
		char[] digitsArray = strNum.toCharArray();

		for (char c : digitsArray) {
			if ((c == '0') || (digitsSet.contains(c))) {
				return false;
			}
			digitsSet.add(c);
		}

		return (digitsSet.size() == 9);
	}

	private boolean isValid(BigInteger n) {
		String strNum = n.toString();
		System.out.printf("%,5d%n", strNum.length());

		return (strNum.length() >= 9)// 
				&& this.isPandigital(strNum.substring(strNum.length() - 9)) //
//				&& this.isPandigital(strNum.substring(0, 9))
				;
	}

	public static void main(String[] args) {
		Problem104Java problem104Java = new Problem104Java();

		int n = 3;
		BigInteger fn;
		BigInteger fn1 = BigInteger.ONE;
		BigInteger fn2 = BigInteger.ONE;

		long t0 = System.currentTimeMillis();
		while (true) {
			System.out.printf("%,8d: ", n);
			fn = fn1.add(fn2);
			if (problem104Java.isValid(fn)) {
				break;
			}
			fn2 = fn1;
			fn1 = fn;
			n++;
		}
		long deltaT = System.currentTimeMillis() - t0;

		System.out.println("==============================");
		System.out.println("Resul: " + n);
		System.out.println("Tempo: " + deltaT + " ms");
	}

}
