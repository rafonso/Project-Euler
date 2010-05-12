package eulerProject.solved;

import java.math.BigInteger;

public class SquareRoot {
	
	public static BigInteger squareRoot(BigInteger testSubject, boolean round) {
		int digitsInTestSubject = testSubject.toString().length();

		double sPoint = (double) digitsInTestSubject / 2.0;

		BigInteger startPoint = BigInteger.valueOf(Math.round(sPoint));
		BigInteger lastGuess = null;
		BigInteger guess = null;

		BigInteger lower = null;
		BigInteger upper = null;

		if (digitsInTestSubject < 3) {
			lastGuess = BigInteger.valueOf(0L);
			lower = lastGuess;
			guess = BigInteger.valueOf(5L);
			upper = BigInteger.valueOf(10L);

		} else {
			startPoint = startPoint.subtract(BigInteger.valueOf(1L));
			startPoint = pow(BigInteger.valueOf(10L), startPoint);

			lastGuess = startPoint;
			lower = lastGuess;

			guess = startPoint.multiply(BigInteger.valueOf(5L));

			upper = startPoint.multiply(BigInteger.valueOf(10L));
		}

		int guesses = 0;
		while (true) {
			guesses++;
			BigInteger ans = guess.pow(2);

			if (ans.compareTo(testSubject) == 0) {
				break;
			}

			if (lastGuess.compareTo(guess) == 0) {
				if (round) {
					if (guess.compareTo(testSubject) == 1) {
						guess = guess.subtract(BigInteger.valueOf(1));
					} else {
						guess = guess.add(BigInteger.valueOf(1));
					}
				} else {
					guess = null;
				}
				break;
			}

			if (ans.compareTo(testSubject) == 1) {
				BigInteger tmp;

				if (guess.compareTo(lastGuess) == 1) {
					upper = guess;
					tmp = upper.subtract(lower);
					tmp = tmp.divide(BigInteger.valueOf(2L));
					tmp = lower.add(tmp);
				} else {
					upper = guess;
					tmp = upper.subtract(upper.subtract(lower).divide(
							BigInteger.valueOf(2L)));
				}

				lastGuess = guess;
				guess = tmp;
			} else {
				BigInteger tmp;
				if (guess.compareTo(lastGuess) == 1) {
					lower = guess;
					tmp = upper.subtract(lower);
					tmp = tmp.divide(BigInteger.valueOf(2L));
					tmp = upper.subtract(tmp);
				} else {
					lower = guess;
					tmp = lower.add(upper.subtract(lower).divide(
							BigInteger.valueOf(2L)));
				}

				lastGuess = guess;
				guess = tmp;
			}
		}

		return guess;
	}

	public static BigInteger pow(BigInteger testSubject, BigInteger pow) {
		BigInteger index = BigInteger.valueOf(1L);
		BigInteger retVal = BigInteger.valueOf(10L);

		while (index.compareTo(pow) != 0) {
			retVal = retVal.multiply(testSubject);
			index = index.add(BigInteger.valueOf(1L));
		}

		return retVal;
	}
}
