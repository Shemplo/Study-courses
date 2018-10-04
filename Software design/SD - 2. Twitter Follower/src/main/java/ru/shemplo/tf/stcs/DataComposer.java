package ru.shemplo.tf.stcs;

import java.util.List;

import ru.shemplo.dsau.utils.TimeDelta;

public interface DataComposer <T, N extends Number> {

	public List <N> compose (List <T> data);
	
	public TimeDelta getCluster ();
	
}
