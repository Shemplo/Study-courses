package ru.shemplo.tf.stcs;

import java.util.List;

public interface DataComposer <T, N extends Number> extends HasTimePeriod {

	public List <N> compose (List <T> data);
	
}
