package eulerProject.solved;

import static java.util.Calendar.*;
import java.util.GregorianCalendar;

/**
 * Problem 19: How many Sundays fell on the first of the month during the
 * twentieth century?<br>
 * 14 June 2002<br>
 * <br>
 * You are given the following information, but you may prefer to do some
 * research for yourself.<br>
 * <ul>
 * <li>1 Jan 1900 was a Monday.
 * <li>Thirty days has September, April, June and November. All the rest have
 * thirty-one, Saving February alone, Which has twenty-eight, rain or shine. And
 * on leap years, twenty-nine.
 * <li>A leap year occurs on any year evenly divisible by 4, but not on a
 * century unless it is divisible by 400.
 * </ul>
 * How many Sundays fell on the first of the month during the twentieth century
 * (1 Jan 1901 to 31 Dec 2000)?<br>
 * 
 * @author rafael
 * EULER: SOLVED
 */
public class Problem019 {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		GregorianCalendar today = new GregorianCalendar(1901, JANUARY, 1);
		final GregorianCalendar end = new GregorianCalendar(2000, DECEMBER, 31);

		int acc = 0;
		while (today.compareTo(end) <= 0) {
			if ((today.get(DATE) == 1) && (today.get(DAY_OF_WEEK) == SUNDAY)) {
				System.out.println(today.getTime());
				acc++;
			}
			today.add(DATE, 1);
		}

		System.out.println();
		System.out.println(acc);
	}

}
