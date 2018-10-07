package ru.shemplo.tf.gfx;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import ru.shemplo.snowball.stuctures.Pair;
import ru.shemplo.snowball.utils.time.TimePeriod;
import ru.shemplo.snowball.utils.time.TimeUtils;
import ru.shemplo.tf.stcs.DataComposer;
import ru.shemplo.tf.stcs.StatisticsData;

public class ImageResultProducer implements ResultProducer <BufferedImage> {

	private final StatisticsData PROVIDER;
	
	private final DateFormat DRAW_TIME_FORMAT = new SimpleDateFormat ("HH:mm"),
							 DRAW_DATE_FORMAT = new SimpleDateFormat ("dd.MM.yy");
	
	private final int MAX_HEIGHT = 200, TEXT_HEIGHT = 15, TEXT_PADDING = 5, LINES = 10;
	private final int PADDING = 20, COLUMN_WIDTH = 20, COLUMN_MARGIN = 15, LEGEND = 30;
	
	public ImageResultProducer (StatisticsData provider) {
		if (provider == null) {
			String text = StatisticsData.class.getSimpleName () + " argument can't be NULL";
			throw new IllegalArgumentException (text);
		}
		
		this.PROVIDER = provider;
	}
	
	private Color getGradient (Color base, int time) {
		double mod = 0.875 + Math.sin (time) / 6;
		int r = (int) (base.getRed   () * mod), 
			g = (int) (base.getGreen () * mod),
			b = (int) (base.getBlue  () * mod),
			a = (int) (base.getAlpha ());
		return new Color (r, g, b, a);
	}
	
	@Override
	public BufferedImage produce (DataComposer <Date, Integer> composer) {
		List <Pair <Date, Integer>> usages = PROVIDER.getUsages (composer);
		/* XXX: REMOVE IN FUTURE */ System.out.println (usages);
		
		TimePeriod period = PROVIDER.getPeriod ();
		Date from = period.F, to = TimeUtils.floorToHours (period.S.getTime ());
		
		TimePeriod trimmedPeriod = TimePeriod.mtp (from, to);
		usages = usages.stream ()
			   . filter (p -> trimmedPeriod.contains (p.F))
			   . collect (Collectors.toList ());
		double limit = usages.stream ().map (p -> p.S).max (Integer::compare).orElse (1);
		
		int columns = usages.size ();
		int width  = Math.max (columns * COLUMN_WIDTH + (columns - 1) * COLUMN_MARGIN + 2 * PADDING + LEGEND, 650), 
			height = MAX_HEIGHT + (TEXT_PADDING + TEXT_HEIGHT + PADDING) * 3;
		
		BufferedImage image = new BufferedImage (width, height, BufferedImage.TYPE_INT_ARGB);
		Graphics g = image.createGraphics ();
		g.setFont (new Font ("Arial", Font.PLAIN, 10));
		
		//g.setColor (Color.WHITE);
		//g.fillRect (0, 0, width, height);
		
		int textOffset = TEXT_PADDING + TEXT_HEIGHT,
			labelsY = textOffset + PADDING + MAX_HEIGHT;
		
		for (int i = 0; i <= LINES; i ++) {
			g.setColor (new Color (200, 200, 200));
			int y = PADDING + textOffset + i * (MAX_HEIGHT / LINES);
			g.drawLine (PADDING, y, width - PADDING - LEGEND, y);
			
			String text = "" + (int) (limit * (1.0 * (LINES - i) / LINES));
			g.setColor (new Color (100, 100, 100));
			g.drawString (text, width - PADDING - LEGEND + 10, y + 3);
		}
		
		for (int i = 0; i < usages.size (); i++) {
			int x = PADDING + i * (COLUMN_WIDTH + COLUMN_MARGIN),
				h = (int) (usages.get (i).S / limit * MAX_HEIGHT);
			
			g.setColor (getGradient (new Color (0, 172, 237), x));
			g.fillRect (x, labelsY - h, COLUMN_WIDTH, h);

			g.setColor (Color.DARK_GRAY);
			g.drawRect (x, labelsY - h, COLUMN_WIDTH, h);
			
			g.setColor (Color.BLACK);
			String text = DRAW_TIME_FORMAT.format (usages.get (i).F);
			int textWidth = g.getFontMetrics ().stringWidth (text);
			g.drawString (text, x + (COLUMN_WIDTH - textWidth) / 2, labelsY + textOffset);
			
			if ("00:00".equals (text) && i != 1 && i != usages.size () - 1) {
				text = DRAW_DATE_FORMAT.format (usages.get (i).F);
				textWidth = g.getFontMetrics ().stringWidth (text);
				g.drawString (text, x + (COLUMN_WIDTH - textWidth) / 2, labelsY + textOffset * 2);
			}
		}
		
		int x = PADDING;
		String text = "Statisctics of using #" + PROVIDER.getRequestKey () 
								+ " in posts of " + PROVIDER.getSource ();
		g.setColor (Color.BLACK);
		g.setFont (new Font ("Courier New", Font.PLAIN, 18));
		g.drawString (text, x, PADDING);
		
		x = PADDING + 0 * (COLUMN_WIDTH + COLUMN_MARGIN);
		text = DRAW_DATE_FORMAT.format (usages.get (0).F);
		g.setFont (new Font ("Arial", Font.PLAIN, 10));
		int textWidth = g.getFontMetrics ().stringWidth (text);
		g.drawString (text, x + (COLUMN_WIDTH - textWidth) / 2, labelsY + textOffset * 2);
		
		x = PADDING + (columns - 1) * (COLUMN_WIDTH + COLUMN_MARGIN);
		text = DRAW_DATE_FORMAT.format (usages.get (columns - 1).F);
		textWidth = g.getFontMetrics ().stringWidth (text);
		g.drawString (text, x + (COLUMN_WIDTH - textWidth) / 2, labelsY + textOffset * 2);
		
		g.dispose ();
		return image;
	}

}
