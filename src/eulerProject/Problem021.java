package eulerProject;

import java.util.ArrayList;
import java.util.List;

/**
 * Problem 21: Evaluate the sum of all amicable pairs under 10000.<br>
 * 05 July 2002<br>
 * <br>
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n
 * which divide evenly into n).<br>
 * If d(a) = b and d(b) = a, where a != b, then a and b are an amicable pair and
 * each of a and b are called amicable numbers.<br>
 * <br>
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
 * 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
 * 71 and 142; so d(284) = 220.<br>
 * <br>
 * <b>Evaluate the sum of all the amicable numbers under 10000.</b>
 * 
 * @author rafael
 * 
 */
public class Problem021 {

	private int getSumOfDivisors(int n) {
		int sum = 0;
//		System.out.printf("%03d => ", n);

		for (int candidate = 1; candidate <= n / 2; candidate++) {
			if (n % candidate == 0) {
				sum += candidate;
//				System.out.printf("%03d ", candidate);
			}
		}
//		System.out.println();

		return sum;
	}

	private int[] getdivisors(int max) {
		int[] sumDivisors = new int[max + 1];
		for (int i = 0; i < sumDivisors.length; i++) {
			sumDivisors[i] = getSumOfDivisors(i);
		}
		return sumDivisors;
	}

	public List<Integer> getAmicables(int[] d) {
		List<Integer> amicables = new ArrayList<Integer>();

		for (int a = 0; a < d.length; a++) {
			int b = d[a];
			if (b <= d.length - 1) {
				if (a == d[b]) {
					amicables.add(a);
				}
			} else if (a == getSumOfDivisors(b)) {
//				System.out.println(a);
				amicables.add(a);
			}
		}

		return amicables;
	}

	private int getSum(List<Integer> amicables) {
		int sum = 0;
		for (Integer amicable : amicables) {
			sum += amicable;
		}
		return sum;
	}

	public static void main(String[] args) {
		int n = Integer.parseInt(args[0]);
		if (n < 0) {
			throw new IllegalArgumentException("Irregular n: " + n);
		}

		Problem021 appl = new Problem021();
		long t0 = System.currentTimeMillis();
		int[] sumDivisors = appl.getdivisors(n);
//		System.out.println(Arrays.toString(sumDivisors));
		List<Integer> amicables = appl.getAmicables(sumDivisors);
		System.out.println(amicables);
		int sum = appl.getSum(amicables);
		long deltaT = System.currentTimeMillis() - t0;
		
		System.out.println("sum(" + n + ") = " + sum);
		System.out.println("Total Time: " + deltaT + " ms");

	}
}
