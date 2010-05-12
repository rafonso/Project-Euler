package eulerProject;

import java.util.HashMap;
import java.util.Map;

/**
 * Problem 14: Find the longest sequence using a starting number under one million.<br>
 * 05 April 2002<br>
 * <br>
 * The following iterative sequence is defined for the set of positive integers:<br>
 * <br>
 * n -> n/2 (n is even)<br>
 * n -> 3n + 1 (n is odd)<br>
 * <br>
 * Using the rule above and starting with 13, we generate the following sequence:<br>
 * 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1<br>
 * <br>
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 
 * 10 terms. Although it has not been proved yet (Collatz Problem), it is 
 * thought that all starting numbers finish at 1.<br>
 * <br>
 * <b>Which starting number, under one million, produces the longest chain?</b><br>
 * <br>
 * NOTE: Once the chain starts the terms are allowed to go above one million.<br>
 * <br>
 * EULER: SOLVED
 */
public class Problem014Java {
	
	private Map<Integer, Long> lengthByTerm = new HashMap<Integer, Long>();
	
	
	
	public Problem014Java() {
		this.lengthByTerm.put(1, 1L);
	}

	private boolean isEven(long n) {
		return (n % 2 == 0);
	}
	
	private long getNextTerm(long n) {
		return isEven(n)? n / 2: 3 * n + 1;
	}
	
	public int calculateSequenceLengthFor(final int n, final int[] lengths) {
		int length = 0;
		long term = n;
		
		boolean found = false;
		while(!found) {
			if(term == 1) {
				found = true;
			} else if((term < lengths.length) && (lengths[(int)term] != 0)) {
				length += lengths[(int)term];
				found = true;
			} else {
				term = getNextTerm(term);
				length ++;
			}
		}
		
		return length;
	}
	
	public int getTermWithGreatestLength(final int start, final int end) {
		int[] lengths = new int[end + 1];
		lengths[1] = 1;
		
		int max = 0;
		int maxLength = 0;
		
		for(int i = start; i <= end; i ++) {
			int length = calculateSequenceLengthFor(i, lengths);
			if(i % 10000 == 0) {
				System.out.println(i);
			}
			if(length > maxLength) {
				System.out.printf("size(%,7d) = %,d %n", i, length);
				max = i; 
				maxLength = length;
			}
			if(lengths[i] == 0) {
				lengths[i] = length;
			}
		}
//		System.out.println(Arrays.toString(lengths));
		return max;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		final int start = 1;
		final int end = 1000000;
		
		Problem014Java problem014Java = new Problem014Java();
		
		long t0 = System.currentTimeMillis();
		int result = problem014Java.getTermWithGreatestLength(start, end);
		long deltaT = System.currentTimeMillis() - t0;
		
		System.out.println("=========================================================");
		System.out.println("Max size for " + end + ": " + result);
		System.out.println("Time: " + deltaT + " ms");
	}

}
