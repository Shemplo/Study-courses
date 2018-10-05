package ru.shemplo.tf;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import ru.shemplo.dsau.utils.time.TimeDelta;
import ru.shemplo.dsau.utils.time.TimeDelta.TDUnit;
import ru.shemplo.tf.gfx.ImageResultProducer;
import ru.shemplo.tf.gfx.ResultProducer;
import ru.shemplo.tf.ntwk.NetworkSession;
import ru.shemplo.tf.ntwk.VKSession;
import ru.shemplo.tf.stcs.HoursComposer;
import ru.shemplo.tf.stcs.StatisticsProvider;

public class Run {

	public static void main (String ... args) throws IOException {
		PropertiesLoader.load ("src/main/resources/properties");
		
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
		
		TimeDelta delta = TimeDelta.valueOf (TDUnit.HWR, -hours);
		
		System.out.println (hashtag + " " + hours);
		NetworkSession session = new VKSession ("shemplo");
		session.tryConnect ();
		
		if (session.isConnected ()) {
			StatisticsProvider provider = session.sendRequest (hashtag, delta.getPeriod ());
			ResultProducer <BufferedImage> producer = new ImageResultProducer (provider);
			BufferedImage image = producer.produce (new HoursComposer ());
			
			ImageIO.write (image, "png", new File ("pic.png"));
		} else {
			System.err.println ("Not connected");
		}
	}
	
}
