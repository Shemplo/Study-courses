package ru.ifmo.droid2016.tmdb.loader;

import android.text.TextUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.w3c.dom.Text;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import ru.ifmo.droid2016.tmdb.model.Movie;
import ru.ifmo.droid2016.tmdb.utils.IOUtils;

/**
 * Created by Андрей on 21.11.2016.
 */

public class RequestParser {

    public static List<Movie> parse (InputStream input) throws IOException,
                                                                JSONException,
                                                                BadResponseException {

        String content  = IOUtils.readToString (input, "UTF-8");
        JSONObject json = new JSONObject (content);

        ArrayList <Movie> list = new ArrayList <> ();
        JSONArray jsonArray    = json.getJSONArray ("results");

        for (int i = 0; i < jsonArray.length(); i ++) {
            JSONObject movie = jsonArray.optJSONObject (i);

            if (movie != null) {
                String poster    = movie.optString ("poster_path", null);
                String title     = movie.optString ("original_title", null);
                String locTitle  = movie.optString ("title", null);
                String overview  = movie.optString ("overview", null);
                String voteRange = movie.optString ("vote_average", null);
                String release   = movie.optString ("release_date", null);

                if (!TextUtils.isEmpty (poster) && !TextUtils.isEmpty (title)) {
                    list.add (new Movie (poster, title, overview, locTitle, voteRange, release));
                }
            }
        }

        return list;
    }

}
