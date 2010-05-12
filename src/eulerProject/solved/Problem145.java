package eulerProject.solved;

import java.util.HashSet;
import java.util.Set;

/**
 * Problem 145: How many reversible numbers are there below one-billion?<br>
 * 16 March 2007<br>
 * <br>
 * Some positive integers n have the property that the sum [ n + reverse(n) ]
 * consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409
 * + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904
 * are reversible. Leading zeroes are not allowed in either n or reverse(n).<br>
 * <br>
 * There are 120 reversible numbers below one-thousand.<br>
 * <br>
 * <b>How many reversible numbers are there below one-billion (10^(9))?<b><br>
 * <br>
 * 
 * @author Magna Sistemas
 */
public class Problem145 {

	private int getFirstDigit(int n) {
		if (n < 10)
			return n / 1;
		if (n < 100)
			return n / 10;
		if (n < 1000)
			return n / 100;
		if (n < 10000)
			return n / 1000;
		if (n < 100000)
			return n / 10000;
		if (n < 1000000)
			return n / 100000;
		if (n < 10000000)
			return n / 1000000;
		if (n < 100000000)
			return n / 10000000;
		if (n < 1000000000)
			return n / 100000000;
		if (n < 2000000000)
			return n / 1000000000;

		throw new IllegalArgumentException("Number to large: " + n);
	}

	private int getLastDigit(int n) {
		return n % 10;
	}

	private boolean isOdd(int d) {
		return (d % 2 != 0);
	}

	private boolean isValid(int n) {
		if (n % 10 == 0) {
			return false;
		}

		int firstDigit = getFirstDigit(n);
		int lastDigit = this.getLastDigit(n);
		boolean fistOddLastEven = (isOdd(firstDigit) && !isOdd(lastDigit));
		boolean fistEvenLastOdd = (!isOdd(firstDigit) && isOdd(lastDigit));
		return fistOddLastEven || fistEvenLastOdd;
	}

	private int getReverse(final int n) {
		int reverse = 0;
		int number = n;

		while (number > 0) {
			reverse = reverse * 10 + number % 10;
			number /= 10;
		}

		return reverse;
	}

	private boolean areReversibles(int n, int reverse) {
		int sum = n + reverse;

		while (sum > 0) {
			int lastDigit = sum % 10;
			if (!this.isOdd(lastDigit)) {
				return false;
			}
			sum /= 10;
		}

		return true;
	}

	private int getQuantityOfReversibles(int max) {
		Set<Integer> reversibles = new HashSet<Integer>();
		Set<Integer> notReversibles = new HashSet<Integer>();

		for (int i = 0; i <= max; i++) {
			if (i % 10000000 == 0) {
				System.out.println("\t" + i);
			}
			Integer bigI = new Integer(i);
			if (reversibles.contains(bigI) || notReversibles.contains(bigI)) {
				continue;
			}

			if (this.isValid(i)) {
				int reverse = this.getReverse(i);
				if (this.areReversibles(i, reverse)) {
					System.out.println(i);
					reversibles.add(bigI);
					reversibles.add(new Integer(reverse));
				} else {
					// notReversibles.add(bigI);
					// notReversibles.add(new Integer(reverse));
				}
			}
		}

		return reversibles.size();
	}

	public static void main(String[] args) {
		Problem145 appl = new Problem145();
		final int max = 1000000000;

		long t0 = System.currentTimeMillis();
		int result = appl.getQuantityOfReversibles(max);
		long deltaT = System.currentTimeMillis() - t0;

		System.out.println("=========================");
		System.out.println(result);
		System.out.println("TIME = " + deltaT);
	}
}
