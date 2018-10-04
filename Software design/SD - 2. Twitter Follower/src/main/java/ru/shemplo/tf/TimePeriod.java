package ru.shemplo.tf;

import java.util.Date;

import ru.shemplo.dsau.stuctures.Pair;

public class TimePeriod extends Pair <Date, Date> {

	public TimePeriod (Date from, Date to) {
		super (from, to);
	}
	
	public boolean contains (Date date) {
		return (F.before (date) || F.equals (date))
			&& (S.after (date) || S.equals (date));
	}
	
	@Override
	public TimePeriod swap () {
		return TimePeriod.mtp (S, F);
	}
	
	public static TimePeriod mtp (Date from, Date to) {
		return new TimePeriod (from, to);
	}
	
}
