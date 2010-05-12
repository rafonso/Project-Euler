package eulerProject.solved;

import java.math.BigInteger;

/**
 * Problem 16 <br>
 * 03 May 2002<br>
 * <br>
 * 2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.<br>
 * <br>
 * What is the sum of the digits of the number 2^(1000)?<br>
 * 
 * @author rafael
 * 
 */
public class Sum2To1000 {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		long t0 = System.currentTimeMillis();
		final BigInteger two = new BigInteger("2", 10);
		final BigInteger value = two.pow(1000);
		char[] chars = value.toString().toCharArray();
		int sum = 0;
		for (char c : chars) {
			sum += (c - '0');
		}
		long deltaT = System.currentTimeMillis() - t0;

		System.out.println(value);
		System.out.println(sum);
		System.out.println("Time: " + deltaT + " ms");
	}

}
