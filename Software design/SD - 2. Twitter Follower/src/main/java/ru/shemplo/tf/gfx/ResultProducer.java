package ru.shemplo.tf.gfx;

import java.util.Date;

import ru.shemplo.tf.stcs.DataComposer;

public interface ResultProducer <T> {

	public T produce (DataComposer <Date, Integer> composer);
	
}
