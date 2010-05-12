/**
 * 
 */
package eulerProject.solved;

import java.util.ArrayList;
import java.util.List;

/**
 * Problem 5.<br>
 * 30 November 2001<br>
 * <br>
 * 2520 is the smallest number that can be divided by each of the numbers from 1
 * to 10 without any remainder.<br>
 * <br>
 * What is the smallest number that is evenly divisible by all of the numbers
 * from 1 to 20?<br>
 * 
 * 
 * @author rafael
 * EULER: SOLVED
 */
public class Problem005 {

	private boolean isPrime(int i, Iterable<Integer> priorPrimes) {
		for (Integer prime : priorPrimes) {
			if (Math.sqrt(prime) > i) {
				return true;
			}
			if (i % prime == 0) {
				return false;
			}
		}

		return true;
	}

	private int getNextPrime(List<Integer> priorPrimes) {
		int candidate = priorPrimes.get(priorPrimes.size() - 1);
		if(candidate == 2) {
			candidate --;
		}

		boolean found = false;
		while (!found) {
			candidate += 2;
			found = isPrime(candidate, priorPrimes);
		}

		return candidate;
	}

	private final int[] prepareNumbers(int n) {
		int[] numbers = new int[n + 1];
		for (int i = 0; i < numbers.length; i++) {
			numbers[i] = i;
		}
		numbers[0] = 1;
		return numbers;
	}

	private int divideAll(int[] values, int prime) {
		int value = 1;
		boolean dividedSome = true;
		while (dividedSome) {
			dividedSome = false;
			for (int i = 1; (i < values.length); i++) {
				if (values[i] % prime == 0) {
					values[i] /= prime;
					dividedSome = true;
				}
			}
			if(dividedSome) {
				value *= prime;
			}
		}

		return value;
	}

	private boolean isAllOne(int[] values) {
		for (int i : values) {
			if (i != 1) {
				return false;
			}
		}
		return true;
	}

	private int getMinimumMultiple(int n) {
		int[] numbers = prepareNumbers(n);
		int value = 1;
		List<Integer> primes = new ArrayList<Integer>();
		primes.add(2);

		// Divide by two
		value *= this.divideAll(numbers, 2);
		
		while(!isAllOne(numbers)) {
			int nextPrime = this.getNextPrime(primes);
			value *= this.divideAll(numbers, nextPrime);
			primes.add(nextPrime);
		}

		return value;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int n = Integer.parseInt(args[0]);
		Problem005 appl = new Problem005();

		long t0 = System.currentTimeMillis();
		int result = appl.getMinimumMultiple(n);
		long deltaT = System.currentTimeMillis() - t0;

		System.out.printf("Minimum multiple for 1 to %d is %d%n", n, result);
		System.out.println("Time: " + deltaT + " ms");
	}

}
