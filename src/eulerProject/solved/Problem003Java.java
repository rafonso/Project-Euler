package eulerProject.solved;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import static java.math.BigInteger.*;

/**
 * Problem 3.<br>
 * 02 November 2001<br>
 * <br>
 * The prime factors of 13195 are 5, 7, 13 and 29.<br>
 * <br>
 * What is the largest prime factor of the number 600851475143 ?<br>
 * 
 * @author rafael
 * 
 */
public class Problem003Java {

	private static final BigInteger TWO = new BigInteger("2");

	private static boolean isPrime(BigInteger i,
			Iterable<BigInteger> priorPrimes) {
		for (BigInteger prime : priorPrimes) {
			if (prime.multiply(prime).compareTo(i) > 0) {
				return true;
			}
			if (i.remainder(prime).equals(ZERO)) {
				return false;
			}
		}
		return true;
	}

	private static BigInteger reduce(BigInteger x, BigInteger prime) {
		if (!x.remainder(prime).equals(ZERO)) {
			throw new IllegalArgumentException(x + " % " + prime + " = "
					+ x.remainder(prime));
		}

		BigInteger result = x;
		do {
			result = result.divide(prime);
		} while (result.remainder(prime).equals(ZERO));
		return result;
	}

	private static BigInteger getGretestPrimeFor(BigInteger n) {
		BigInteger value = (n.remainder(TWO).equals(ZERO)) ? reduce(n, TWO) : n;
		if (value.equals(ONE)) {
			return TWO;
		}

		List<BigInteger> primes = new ArrayList<BigInteger>();
		primes.add(new BigInteger("3"));
		BigInteger gretestPrime = ONE;
		boolean find = false;

		BigInteger candidate = new BigInteger("3");
		while (!find) {
			if (isPrime(candidate, primes)) {
				primes.add(candidate);
				if (value.remainder(candidate).equals(ZERO)) {
					System.out.print(candidate + " ");
					gretestPrime = candidate;
					value = reduce(value, candidate);
					find = value.equals(ONE);
				}
			}
			candidate = candidate.add(TWO);
		}
		System.out.println();
		return gretestPrime;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		BigInteger n = new BigInteger(args[0]);
//		BigInteger prime = new BigInteger(args[1]);
		
		long t0 = System.currentTimeMillis();
		System.out.println(getGretestPrimeFor(n));
		long deltaT = System.currentTimeMillis() - t0;
		System.out.println("Time: " + deltaT + " ms");
	}

}
