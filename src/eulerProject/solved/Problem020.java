package eulerProject.solved;

import java.math.BigInteger;

/**
 * Problem 20. <br>
 * 21 June 2002<br>
 * <br>
 * n! means n*(n-1)*...*3*2*1 <br>
 * Find the sum of the digits in the number 100!<br>
 * 
 * @author rafael
 * EULER: SOLVED
 */
public class Problem020 {

	private static BigInteger getFactorial(int n) {
		BigInteger factorial = BigInteger.ONE;

		for (int i = 1; i <= n; i++) {
			factorial = factorial.multiply(new BigInteger(String.valueOf(i)));
		}

		return factorial;
	}

	private static int sumDigits(Number n) {
		int sum = 0;
		char[] digits = n.toString().toCharArray();

		for (char c : digits) {
			sum += (c - '0');
		}

		return sum;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int n = Integer.parseInt(args[0]);

		long t0 = System.currentTimeMillis();
		BigInteger factorial = getFactorial(n);
		int sumDigits = sumDigits(factorial);
		long deltaT = System.currentTimeMillis() - t0;

		System.out.println(n + "! = " + factorial);
		System.out.println("Sum Digits = " + sumDigits);
		System.out.println("Time: " + deltaT + " ms");
	}

}
