package me.shemplo.homework3;

import android.app.Service;
import android.content.AsyncTaskLoader;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.IBinder;
import android.util.Log;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by Андрей on 28.11.2016.
 */

public class ImageService extends Service {

    private boolean inProcess = false;
    private String [] images  = new String [] {
            "http://galereika.org/_ph/139/993871604.jpg",
            "http://cs6028.vk.me/v6028485/765d/B36bXvd8IgA.jpg",
            "http://i47.beon.ru/87/33/1253387/63/78646863/fhdbth.jpeg",
            "http://luxfon.com/pic/201407/640x960/luxfon.com-32397.jpg",
            "http://byaki.net/uploads/posts/2014-10/1413406872_funny_animals_01400_036.jpg"
    };

    public int onStartCommand (final Intent intent, int flags, int startID) {
        Log.d ("IMAGE_S", "Service `" + this.getClass().getName() + "` is started");

        if (inProcess) { return START_STICKY; }

        //Set that service started to load
        inProcess = true;

        //Image storage file
        final File main = new File (getFilesDir (), MainActivity.IMAGE);

        //Get the URL to image or number in base
        String sms = intent.getExtras ().getString ("sms");
        Log.d ("IMAGE_S", "Sms `" + sms + "` is received");

        //Try to fetch a URL
        String tmp = null;
        Integer number = 0;
        if ((tmp = _fetchURL (sms)) == null) {
            if ((number = _fetchNumber (sms)) != -1) {
                tmp = images [Math.abs (number - 1) % images.length];
            } else {
                Random random = new Random ();
                tmp = images [random.nextInt (images.length)];
            }
        }

        //Set it final to provide to AsyncTask
        final String url = tmp;

        //Loading and saving image in file
        new AsyncTask <Void, Void, Void> () {

            private HttpURLConnection connection;
            private InputStream  input;
            private OutputStream output;

            protected Void doInBackground (Void [] voids) {
                try {
                    Log.d ("IMAGE_S", "Try to connect to `" + url + "` to get image");
                    connection = (HttpURLConnection) new URL (url).openConnection ();
                    connection.setReadTimeout (30 * 1000);

                    //Reading and writing streams from connection and file
                    input  = new BufferedInputStream (connection.getInputStream());
                    output = new FileOutputStream    (main);

                    //Writing data in file
                    int symbol;
                    while ((symbol = input.read ()) != -1) { output.write (symbol); }
                    Log.d ("IMAGE_S", "Service `" + this.getClass().getName()
                                            + "` finished downloading");
                } catch (Exception e) {
                    //Notice that everythig is bad
                    Log.d ("IMAGE_S", "Failed to open connection");

                    //Deleting unfinished file
                    main.delete ();
                }

                try {
                    //Closing everything possible
                    if (connection != null) { connection.disconnect (); }
                    if (input      != null) { input.close  (); }
                    if (output     != null) { output.close (); }
                } catch (Exception e) {
                    //Notice that everythig is bad
                    Log.d ("IMAGE_S", "Failed to close working stream");
                }

                //Notice that image had been changed
                sendBroadcast (new Intent ("ImageLoader"));

                //Set that service finished loading
                inProcess = false;

                return null;
            }

        }.executeOnExecutor (AsyncTask.THREAD_POOL_EXECUTOR);

        return START_STICKY;
    }

    private String _fetchURL (String sms) {
        String url = null;

        String mask = "^(https?:\\/\\/)?([\\w\\.]+)\\.([a-z]{2,6}\\.?)(\\/[\\w\\.]*)*\\/?$";
        Pattern pattern = Pattern.compile (mask);

        ArrayList <String> tokens = new ArrayList <> ();
        StringBuilder token = new StringBuilder ();
        boolean wasSpace = true;

        for (int i = 0; i < sms.length (); i ++) {
            char symbol = sms.charAt (i);

            if (symbol == ' ' && !wasSpace) {
                tokens.add (token.toString ());
                token.setLength (0);
                wasSpace = true;
            } else {
                token.append (symbol);
                wasSpace = false;
            }
        }

        if (token.length() != 0) {
            tokens.add (token.toString ());
        }

        for (int i = 0; i < tokens.size (); i ++) {
            Log.d ("IMAGE_S", "Now token: " + tokens.get (i));
            Matcher matcher = pattern.matcher (tokens.get (i));

            if (matcher.find ()) {
                url = tokens.get (i);
                break;
            }
        }

        return url;
    }

    private int _fetchNumber (String sms) {
        int result = -1;

        try {
            result = Integer.parseInt (sms);
        } catch (Exception e) {}

        return result;
    }

    public IBinder onBind (Intent intent) { return null; }

}
