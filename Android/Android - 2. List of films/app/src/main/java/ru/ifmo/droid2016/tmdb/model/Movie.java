package ru.ifmo.droid2016.tmdb.model;

import android.provider.Settings;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;

/**
 * Информация о фильме, полученная из The Movie DB API
 */

public class Movie {

    private final String BASE_URL = "https://image.tmdb.org/t/p/w500";

    /**
     * Path изображения постера фильма. Как из Path получить URL, описано здесь:
     *
     * https://developers.themoviedb.org/3/getting-started/languages
     *
     * В рамках ДЗ можно не выполнять отдельный запрос /configuration, а использовать
     * базовый URL для картинок: http://image.tmdb.org/t/p/ и
     */
    public final @NonNull String posterPath;

    /**
     * Название фильма на языке оригинала.
     */
    public final @NonNull String originalTitle;

    /**
     * Описание фильма на языке пользователя.
     */
    public final @Nullable String overviewText;

    /**
     * Название фильма на языке пользователя.
     */
    public final @Nullable String localizedTitle;

    public final @Nullable String voteRange;

    public final @Nullable String releaseDate;

    public Movie(String posterPath,
                 String originalTitle,
                 String overviewText,
                 String localizedTitle,
                 String voteRange,
                 String releaseDate) {
        this.posterPath     = BASE_URL + posterPath;

        this.originalTitle  = originalTitle;
        this.overviewText   = overviewText;
        this.localizedTitle = localizedTitle;
        this.voteRange      = voteRange;
        this.releaseDate    = releaseDate;
    }
}
