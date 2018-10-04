package ru.shemplo.tf.gfx;

import java.util.Date;

import java.awt.image.BufferedImage;

import ru.shemplo.tf.stcs.DataComposer;
import ru.shemplo.tf.stcs.StatisticsProvider;

public class ImageResultProducer implements ResultProducer <BufferedImage> {

	private final StatisticsProvider PROVIDER;
	
	public ImageResultProducer (StatisticsProvider provider) {
		this.PROVIDER = provider;
	}
	
	@Override
	public BufferedImage produce (DataComposer <Date, Integer> composer) {
		PROVIDER.getUsages (composer);
		return null;
	}

}
