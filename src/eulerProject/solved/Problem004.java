package eulerProject.solved;

/**
 * 
 * @author rafael
 * EULER: SOLVED
 *
 */
public class Problem004 {

	private static boolean isPalindrome(Integer n) {
		char[] digits = n.toString().toCharArray();
		
		boolean palindorme = true;
		for(int i = 0, j = (digits.length - 1); palindorme && (i < j); i ++, j --) {
			palindorme = (digits[i] == digits[j]);
		}
		return palindorme;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int greatest = 0;
		for(int i = 100; i < 1000; i ++) {
			for(int j = i; j < 1000; j ++) {
				int product = i * j;
				if(isPalindrome(product)) {
					if(greatest < product) {
						greatest = product;
					}
					System.out.println(i + " x " + j + " = " + product + " (" + (product % 11) + ")");
				}
			}
		}
		System.out.println("====================");
		System.out.println(greatest);
	}

}
