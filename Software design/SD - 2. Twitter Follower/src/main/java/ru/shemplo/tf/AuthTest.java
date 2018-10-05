package ru.shemplo.tf;

import com.vk.api.sdk.client.TransportClient;
import com.vk.api.sdk.client.VkApiClient;
import com.vk.api.sdk.client.actors.UserActor;
import com.vk.api.sdk.httpclient.HttpTransportClient;
import com.vk.api.sdk.objects.newsfeed.responses.SearchResponse;

public class AuthTest {

	public static void main (String [] args) throws Exception {
		TransportClient transportClient = HttpTransportClient.getInstance (); 
		VkApiClient vk = new VkApiClient (transportClient);
		
		/*
		UserAuthResponse authResponse = vk.oauth ()
			.userAuthorizationCodeFlow (6089339, "u6gaILJvJO3fu1hgj4RZ", "http://vk.com/blank.html", "22b798b0eeb30966b4")
			.execute ();
		*/
		
		UserActor actor = new UserActor (146436035, 
			"cdbabe14d3cbfa84cb1a7f977eb5036627c045473a1db9b0b4a1233369e2546fdf98386fab7f97fc2196e");
		SearchResponse response = vk.newsfeed ().search (actor).q ("#java").execute ();
		System.out.println (response);
	}

}
