package eulerProject.solved;

import java.math.BigInteger;

/**
 * Problem 48.<br>
 * 18 July 2003<br>
 * <br>
 * The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.<br>
 * <br>
 * Find the last ten digits of the series, 1^(1) + 2^(2) + 3^(3) + ... +
 * 1000^(1000).<br>
 * 
 * @author rafael
 * EULER: SOLVED
 * 
 */
public class Problem048 {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int n = Integer.parseInt(args[0]);
		int lastDigits = Integer.parseInt(args[1]);
		BigInteger sum = BigInteger.ZERO;

		long t0 = System.currentTimeMillis();
		for(int i = 1; i <= n; i ++) {
			sum = sum.add(new BigInteger(String.valueOf(i)).pow(i));
		}
		BigInteger remainder = sum.remainder(BigInteger.TEN.pow(lastDigits));
		long deltaT = System.currentTimeMillis() - t0;
		
		System.out.println("SUM = " + sum);
		System.out.println("REMAINDER = " + remainder);
		System.out.println("Time: " + deltaT + " ms");
	}

}
