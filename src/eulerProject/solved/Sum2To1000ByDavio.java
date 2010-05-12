package eulerProject.solved;

import java.math.BigInteger;

public class Sum2To1000ByDavio {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		long t0 = System.currentTimeMillis();
		BigInteger  n = BigInteger.valueOf(2L).pow(1000);
		int sum = 0;
        int digit;
        while(n.compareTo(BigInteger.ONE)!=-1) {
            digit = n.remainder(BigInteger.TEN).intValue();
            n = n.divide(BigInteger.TEN);
            sum += digit;
//            System.out.println("Sum: "+sum+", Digit Added: "+digit);
        }
        long deltaT = System.currentTimeMillis() - t0;
        
        System.out.println(sum);
        System.out.println("Time: " + deltaT + " ms");
	}

}
