package monotonic_clock;

import org.jetbrains.annotations.NotNull;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 */
public class Solution implements MonotonicClock {
	
    private final RegularInt lc1 = new RegularInt (0);
    private final RegularInt lc2 = new RegularInt (0);
    private final RegularInt lc3 = new RegularInt (0);
    private final RegularInt rc1 = new RegularInt (0);
    private final RegularInt rc2 = new RegularInt (0);
    private final RegularInt rc3 = new RegularInt (0);

    @Override
    public void write (@NotNull Time time) {
		// Write left-to-right
        rc1.setValue (time.getD1 ());
        rc2.setValue (time.getD2 ());
        rc3.setValue (time.getD3 ());
		
		// Write right-to-left
		lc3.setValue (time.getD3 ());
        lc2.setValue (time.getD2 ());
        lc1.setValue (time.getD1 ());
    }

    @NotNull
    @Override
    public Time read () {
		final RegularInt tlc1 = new RegularInt (lc1.getValue ());
		final RegularInt tlc2 = new RegularInt (lc2.getValue ());
		final RegularInt tlc3 = new RegularInt (lc3.getValue ());
		final RegularInt trc3 = new RegularInt (rc3.getValue ());
		final RegularInt trc2 = new RegularInt (rc2.getValue ());
		final RegularInt trc1 = new RegularInt (rc1.getValue ());
		
		int caze = 0;
		if (tlc1.getValue () == trc1.getValue ()) {
			caze = 1;
			if (tlc2.getValue () == trc2.getValue ()) {
				caze = 2;
				if (tlc3.getValue () == trc3.getValue ()) {
					caze = -1;
				}
			}
		}
		
		switch (caze) {
			case 0:
				return new Time (trc1.getValue (), 0, 0);
			case 1:
				return new Time (trc1.getValue (), trc2.getValue (), 0);
			case 2:
				return new Time (trc1.getValue (), trc2.getValue (), trc3.getValue ());
			default:
				return new Time (tlc1.getValue (), tlc2.getValue (), tlc3.getValue ());
		}
    }
	
}
