package ru.shemplo.tf.ntwk;

import java.io.IOException;

import ru.shemplo.tf.TimePeriod;
import ru.shemplo.tf.stcs.StatisticsProvider;

public interface NetworkSession {

	public boolean isConnected ();
	
	public void tryConnect () throws IOException;
	
	public StatisticsProvider sendRequest (String key, TimePeriod period) 
		throws IOException;
	
}
