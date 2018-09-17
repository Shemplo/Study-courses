package ru.ifmo.droid2016.tmdb.loader;

import android.content.Context;
import android.support.v4.content.AsyncTaskLoader;
import android.util.Log;

import com.facebook.stetho.urlconnection.StethoURLConnectionManager;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.util.List;
import java.util.Locale;

import ru.ifmo.droid2016.tmdb.api.TmdbApi;
import ru.ifmo.droid2016.tmdb.model.Movie;
import ru.ifmo.droid2016.tmdb.utils.IOUtils;

/**
 * Created by Андрей on 21.11.2016.
 */

public class RequestLoader extends AsyncTaskLoader<LoadResult <List <Movie>>> {

    private static final String TAG = "Movies";

    public RequestLoader (Context context) {
        super (context);
    }

    protected void onStartLoading () { forceLoad (); }

    public LoadResult <List <Movie>> loadInBackground () {
        final StethoURLConnectionManager streho = new StethoURLConnectionManager ("API");

        ResultType result = ResultType.ERROR;
        List <Movie> answer = null;

        HttpURLConnection connect = null;
        InputStream       input   = null;

        try {
            connect = TmdbApi.getPopularMoviesRequest (Locale.getDefault ().getLanguage(), 1);
            streho.preConnect (connect, null);
            connect.connect();
            streho.postConnect();

            if (connect.getResponseCode () == 200 /*Http OK*/) {
                input = connect.getInputStream ();
                input = streho.interpretResponseStream (input);

                answer = RequestParser.parse (input);
                result = ResultType.OK;
            } else {
                throw new BadResponseException ("[ HTTP ] " + connect.getResponseCode () + ": " + connect.getResponseMessage ());
            }
        } catch (MalformedURLException mue) {
            Log.e (TAG, "Failed load list of movies ", mue);
        } catch (IOException ioe) {
            streho.httpExchangeFailed (ioe);

            if (IOUtils.isConnectionAvailable (getContext (), false)) {
                result =  ResultType.NO_INTERNET;
            } else {
                result = ResultType.ERROR;
            }
        } catch (Exception e) {
            Log.e (TAG, "Unexpected exception ", e);
        } finally {
            IOUtils.closeSilently (input);
            if (connect != null) { connect.disconnect (); }
        }

        return new LoadResult <> (result, answer);
    }

}
