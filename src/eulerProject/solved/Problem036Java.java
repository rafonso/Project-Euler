package eulerProject.solved;

public class Problem036Java {
	
	private boolean isPalindrom(String s) {
		char[] chars = s.toCharArray();
		int i = 0, j = chars.length - 1;
		
		while(i < j) {
			if(chars[i ++] != chars[j --]) {
				return false;
			}
		}
		return true;
	}
	
	private boolean isPalindrom(long l) {
		return isPalindrom(String.valueOf(l));
	}
	
	public long sumDecimalAndBinaryPalidromes(long n) {
		long sum = 0L;
		
		for(long i = 0; i < n; i ++) {
			if(isPalindrom(i) && isPalindrom(Long.toBinaryString(i))) {
				System.out.println(i + " (" + Long.toBinaryString(i) + ")");
				sum += i;
			}
		}
		
		return sum;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		long n = Long.parseLong(args[0]);
		Problem036Java appl = new Problem036Java();

		long t0 = System.currentTimeMillis();
		long result = appl.sumDecimalAndBinaryPalidromes(n);
		long deltaT = System.currentTimeMillis() - t0;
		
		System.out.println(result);
		System.out.println("Total Time: " + deltaT + " ms");

	}

}
/*
PALINDOMS FROM 1 to 10^9
1 (1)
3 (11)
5 (101)
7 (111)
9 (1001)
33 (100001)
99 (1100011)
313 (100111001)
585 (1001001001)
717 (1011001101)
7447 (1110100010111)
9009 (10001100110001)
15351 (11101111110111)
32223 (111110111011111)
39993 (1001110000111001)
53235 (1100111111110011)
53835 (1101001001001011)
73737 (10010000000001001)
585585 (10001110111101110001)
1758571 (110101101010101101011)
1934391 (111011000010000110111)
1979791 (111100011010110001111)
3129213 (1011111011111101111101)
5071705 (10011010110001101011001)
5259525 (10100000100000100000101)
5841485 (10110010010001001001101)
13500531 (110011100000000001110011)
719848917 (101010111010000000010111010101)
910373019 (110110010000110011000010011011)
939474939 (110111111111110011111111111011)

2609044274
Time = 286397 ms
*/