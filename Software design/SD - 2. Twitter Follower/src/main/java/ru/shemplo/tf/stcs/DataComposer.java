package ru.shemplo.tf.stcs;

import java.util.List;

import ru.shemplo.dsau.stuctures.Pair;

public interface DataComposer <T, N extends Number> extends HasTimePeriod {

	public List <Pair <T, N>> compose (List <T> data);
	
}
