package ru.shemplo.tf.ntwk;

import java.io.IOException;

import java.util.ArrayList;
import java.util.List;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.vk.api.sdk.client.ClientResponse;
import com.vk.api.sdk.client.TransportClient;
import com.vk.api.sdk.client.VkApiClient;
import com.vk.api.sdk.client.actors.UserActor;
import com.vk.api.sdk.exceptions.ClientException;
import com.vk.api.sdk.httpclient.HttpTransportClient;
import com.vk.api.sdk.queries.newsfeed.NewsfeedSearchQuery;

import ru.shemplo.snowball.utils.time.TimePeriod;
import ru.shemplo.snowball.utils.time.TimeUtils;
import ru.shemplo.tf.stcs.StatisticsData;
import ru.shemplo.tf.stcs.VKStatisticsData;

public class VKSession implements NetworkSession {

	private final UserActor ACTOR;
	private VkApiClient client;
	
	public VKSession (String userLogin) {
		int UID = Integer.parseInt (System.getProperty ("api.vk." + userLogin + ".uid"));
		String accessToken = System.getProperty ("api.vk." + userLogin + ".token");
		this.ACTOR = new UserActor (UID, accessToken);
	}
	
	@Override
	public boolean isConnected () {
		return this.client != null;
	}

	@Override
	public void tryConnect () throws IOException {
		TransportClient transportClient = HttpTransportClient.getInstance (); 
		this.client = new VkApiClient (transportClient);
		
		/*
		try {
			client.account ().getInfo (ACTOR).execute ();
		} catch (ClientException | ApiException es) {
			this.client = null; // not connected
			
			throw new IOException (es);
		}
		*/
	}

	@Override
	public StatisticsData sendRequest (String key, TimePeriod period) throws IOException {
		if (!isConnected ()) {
			throw new IllegalStateException ("Not connected");
		}
		
		try {
			TimePeriod tmpPeriod = TimePeriod.mtp (TimeUtils.floorToHours (period.F), period.S);
			int startTime = (int) (tmpPeriod.F.getTime () / 1000),
				endTime = (int) (tmpPeriod.S.getTime () / 1000);
			List <JsonObject> posts = new ArrayList <> ();
			
			String startFrom = "";
			do {
				NewsfeedSearchQuery nsq = client.newsfeed ().search (ACTOR)
										. q (key)
										. count (200)
										. startTime (startTime)
										. endTime (endTime)
										. startFrom (startFrom);
				ClientResponse response = nsq.executeAsRaw ();
				JsonElement json = new JsonParser ().parse (response.getContent ());
				JsonObject res = json.getAsJsonObject ().getAsJsonObject ("response");
				
				JsonArray items = res.getAsJsonArray ("items");
				items.forEach (i -> posts.add (i.getAsJsonObject ()));
				
				startFrom = "";
				if (res.has ("next_from")) {
					startFrom = res.getAsJsonPrimitive ("next_from").getAsString ();
				}
			} while (startFrom.length () > 0);
			
			return new VKStatisticsData (key, period, posts);
		} catch (ClientException es) {
			this.client = null;
			
			throw new IOException (es);
		}
	}
	
}
