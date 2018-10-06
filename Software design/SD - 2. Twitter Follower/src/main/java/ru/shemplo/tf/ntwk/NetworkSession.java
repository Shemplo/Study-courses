package ru.shemplo.tf.ntwk;

import java.io.IOException;

import ru.shemplo.dsau.utils.time.TimePeriod;
import ru.shemplo.tf.stcs.StatisticsProvider;

public interface NetworkSession {

	/**
	 * Tests whether session connection is established.
	 * 
	 * This method guarantee nothing except current state of connection.
	 * If it used in <i>if</i> statement then it can be used only for
	 * testing whether connection wasn't established. In positive result
	 * of this method you must be ready to negative result even in small
	 * period of time (because assumed that it depends on Internet connection).
	 * 
	 * @return <i>true</i> if request can be sent to the service (etc.);
	 * 	       <i>false</i> otherwise
	 * 
	 */
	public boolean isConnected ();
	
	/**
	 * Make attempt to connect to service. 
	 * 
	 * The result of this method can be checked with {@link #isConnected ()} 
	 * method. (Assumed that if happened some critical error then it will be
	 * passed throw exception).
	 * 
	 * @throws IOException in case of connection error
	 * 
	 */
	public void tryConnect () throws IOException;
	
	/**
	 * Send request for retrieving data from service.
	 * 
	 * If method {@link #isConnected ()} returned negative result
	 * then attempt must be ignored or must pass throw exception.
	 * 
	 * As result will be returned instance of {@link StatisticsProvider}
	 * that will contains all received data during given {@link TimePeriod}
	 * by the requested <i>key</i>.
	 * 
	 * @param key word that is used for searching in service API
	 * @param period when required key should be searched
	 * 
	 * @return instance of data container
	 * 
	 * @throws IOException in case of request errors
	 * 
	 * @see StatisticsProvider
	 * 
	 */
	public StatisticsProvider sendRequest (String key, TimePeriod period) 
		throws IOException;
	
}
