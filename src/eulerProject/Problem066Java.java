/**
 * 
 */
package eulerProject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * Problem 66: Investigate the Diophantine equation x^2 - D*y^2 = 1.<br>
 * 26 March 2004<br>
 * <br>
 * Consider quadratic Diophantine equations of the form:<br>
 * <br>
 * x^2 - –D*y^2 = 1<br>
 * <br>
 * For example, when D=13, the minimal solution in x is 649^(2) + 13*180^(2) =
 * 1.<br>
 * <br>
 * It can be assumed that there are no solutions in positive integers when D is
 * square.<br>
 * <br>
 * By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
 * following:<br>
 * <br>
 * 3^2 - 2*2^2 = 1<br>
 * 2^2 - 3*1^2 = 1<br>
 * 9^2 - 5*4^2 = 1<br>
 * 5^2 - 6*2^2 = 1<br>
 * 8^2 - 7*3^2 = 1<br>
 * <br>
 * Hence, by considering minimal solutions in x for D <= 7, the largest x is
 * obtained when D = 5.<br>
 * <br>
 * <b>Find the value of D <= 1000 in minimal solutions of x for which the
 * largest value of x is obtained.</b><br>
 * 
 */
public class Problem066Java {

	private final static class DiopahntineComparator implements
			Comparator<Diophantine> {
		@Override
		public int compare(Diophantine d1, Diophantine d2) {
			return d1.getX().compareTo(d2.getX());
		}
	}

	private class Diophantine {

		private Long x, y;
		private int d;
		private long time;

		Diophantine(Long x, Long y, int d, long time) {
			this.x = x;
			this.y = y;
			this.d = d;
			this.time = time;
		}

		Long getX() {
			return x;
		}
/*
		Long getY() {
			return y;
		}

		int getD() {
			return d;
		}

		public long getTime() {
			return time;
		}
*/
		@Override
		public String toString() {
			return "Diophantine [d=" + d + ", x=" + x + ", y=" + y + ", time="
					+ time + "]";
		}

	}

	public class DiophantineCallable implements Callable<Diophantine> {

		private int d;

		public DiophantineCallable(int d) {
			this.d = d;
		}

		@Override
		public Diophantine call() throws Exception {
			return testD(d);
		}

	}

	private long getSqrt(final long n) {
//		long rootAnt = 2 * n, root = n;
//
//		while(rootAnt > root) {
//			rootAnt = root;
//			root = (root + n / root)/2;
//		}
//		
//		return (root * root == n)? root: 0L;
		long root = (long) Math.sqrt(n);
		if (root * root == n) {
			return root;
		} else {
			return 0;
		}
	}

	private Diophantine testD(int d) {
		long y = 0L;
		long x = 0L;

		long t0 = System.currentTimeMillis();
		do {
			y++;
			assert ((Long.MAX_VALUE / d) > y * y);
			x = this.getSqrt(1 + d * y * y);
		} while (x == 0);
		long deltaT = System.currentTimeMillis() - t0;

		final Diophantine diophantine = new Diophantine(x, y, d, deltaT);
		System.out.println(diophantine);
		return diophantine;
	}

	/**
	 * @param args
	 * @throws InterruptedException 
	 * @throws ExecutionException 
	 */
	public static void main(String[] args) throws InterruptedException, ExecutionException {
		final int max = 200;
		Problem066Java problem066Java = new Problem066Java();

		List<DiophantineCallable> callables = new ArrayList<DiophantineCallable>();
		for (int i = 1; i < max; i++) {
			if (problem066Java.getSqrt(i) == 0) {
				callables.add(problem066Java.new DiophantineCallable(i));
			}
		}
		ExecutorService executorService = Executors.newFixedThreadPool(5);
		
		long t0 = System.currentTimeMillis();
		List<Future<Diophantine>> futures = executorService.invokeAll(callables);
		List<Diophantine> diophantines = new ArrayList<Diophantine>();
		for (Future<Diophantine> future : futures) {
			diophantines.add(future.get());
		}
		Diophantine maxDiophantine = Collections.max(diophantines,
				new DiopahntineComparator());
		long deltaT = System.currentTimeMillis() - t0;
		
		/*
		List<Diophantine> diophantines = new ArrayList<Diophantine>();
		long t0 = System.currentTimeMillis();
		for (int i = 1; i < max; i++) {
			if (problem066Java.getSqrt(i) == 0) {
				diophantines.add(problem066Java.testD(i));
			}
		}
		Diophantine maxDiophantine = Collections.max(diophantines,
				new DiopahntineComparator());
		long deltaT = System.currentTimeMillis() - t0;
		*/

		System.out.println("=================================================");
		System.out.println(maxDiophantine);
		System.out.println("Time = " + deltaT + " ms");
//		System.out.println(problem066Java.getSqrt(3037000499976059L));
	}

}
