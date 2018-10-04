package ru.shemplo.tf;

import java.awt.image.BufferedImage;
import java.io.IOException;

import ru.shemplo.dsau.utils.time.TimeDelta;
import ru.shemplo.dsau.utils.time.TimeDelta.TDUnit;
import ru.shemplo.tf.gfx.ImageResultProducer;
import ru.shemplo.tf.gfx.ResultProducer;
import ru.shemplo.tf.ntwk.LocalNetworkSession;
import ru.shemplo.tf.ntwk.NetworkSession;
import ru.shemplo.tf.stcs.HoursComposer;
import ru.shemplo.tf.stcs.StatisticsProvider;

public class Run {

	public static void main (String ... args) throws IOException {
		if (args == null || args.length < 1) {
			String text = "First argument missed: [hashtag]";
			throw new IllegalArgumentException (text);
		}
		
		if (args.length < 2) {
			String text = "Second argument missed: [period in hours]";
			throw new IllegalArgumentException (text);
		}
		
		String hashtag = args [0];
		int hours = 0;
		
		try {
			hours = Math.abs (Integer.parseInt (args [1]));
		} catch (NumberFormatException nfe) {
			String text = "Second argument [period in hours] must be a number";
			throw new IllegalArgumentException (text);
		}
		
		TimeDelta delta = TimeDelta.valueOf (TDUnit.HWR, -20);
		
		System.out.println (hashtag + " " + hours);
		NetworkSession session = new LocalNetworkSession ();
		
		StatisticsProvider provider = session.sendRequest ("tag", delta.getPeriod ());
		ResultProducer <BufferedImage> producer = new ImageResultProducer (provider);
		BufferedImage image = producer.produce (new HoursComposer ());
		
		System.out.println (image);
	}
	
}
