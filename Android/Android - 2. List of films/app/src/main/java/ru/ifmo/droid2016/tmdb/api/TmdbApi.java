package ru.ifmo.droid2016.tmdb.api;

import android.net.Uri;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;

/**
 * Методы для работы с The Movie DB API
 *
 * https://www.themoviedb.org/documentation/api
 */
public final class TmdbApi {

    // TODO: Зарегистрироваться на https://www.themoviedb.org и получить свой собственный ключ

    private static final String API_KEY = "f52695220870f1f2c4be1b995a7ac8e1";
    private static final Uri    BASE_URI = Uri.parse("https://api.themoviedb.org/3");


    private TmdbApi() {}

    /**
     * Возвращает {@link HttpURLConnection} для выполнения запроса популярных фильмов
     *
     * https://developers.themoviedb.org/3/movies/get-popular-movies
     *
     * @param lang язык пользователя
     * @param page страница просмотра
     */
    public static HttpURLConnection getPopularMoviesRequest(String lang, int page) throws IOException {
        Uri uri = BASE_URI.buildUpon()
                    .appendPath("movie")
                    .appendPath("popular")
                    .appendQueryParameter("language", lang)
                    .appendQueryParameter("api_key", API_KEY)
                    .appendQueryParameter("page", page + "").build();

        return (HttpURLConnection) new URL(uri.toString()).openConnection();
    }
}
