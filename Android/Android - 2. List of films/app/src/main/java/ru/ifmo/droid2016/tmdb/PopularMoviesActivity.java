package ru.ifmo.droid2016.tmdb;

import android.os.Bundle;
import android.support.v4.app.LoaderManager;
import android.support.v4.content.Loader;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;

import java.util.List;

import ru.ifmo.droid2016.tmdb.loader.LoadResult;
import ru.ifmo.droid2016.tmdb.loader.RequestLoader;
import ru.ifmo.droid2016.tmdb.loader.ResultType;
import ru.ifmo.droid2016.tmdb.model.Movie;
import ru.ifmo.droid2016.tmdb.utils.RecylcerDividersDecorator;

/**
 * Экран, отображающий список популярных фильмов из The Movie DB.
 */
public class PopularMoviesActivity extends AppCompatActivity
                                    implements LoaderManager.LoaderCallbacks <LoadResult <List <Movie>>> {

    private TextView     log;
    private ProgressBar  progress;
    private RecyclerView recycler;

    private  RecyclerAdapter adapter;

    @Override
    protected void onCreate (Bundle savedInstanceState) {
        super.onCreate (savedInstanceState);
        setContentView (R.layout.activity_popular_movies);

        log      = (TextView)     findViewById (R.id.log);
        progress = (ProgressBar)  findViewById (R.id.progress);
        recycler = (RecyclerView) findViewById (R.id.recycler);

        progress.setIndeterminate  (true);
        recycler.setLayoutManager  (new LinearLayoutManager (this));
        recycler.addItemDecoration (new RecylcerDividersDecorator (getResources ().getColor (R.color.colorPrimaryDark)));

        log.setVisibility      (View.GONE);
        progress.setVisibility (View.GONE);
        recycler.setVisibility (View.GONE);

        getSupportLoaderManager ().initLoader (0, getIntent ().getExtras (), this);
    }

    public Loader <LoadResult <List <Movie>>> onCreateLoader (int id, Bundle args) {
        return new RequestLoader (this);
    }

    public void onLoadFinished (Loader <LoadResult <List <Movie>>> loader, LoadResult <List <Movie>> result) {
        if (result.resultType == ResultType.OK) {
            if (result.data != null && !result.data.isEmpty ()) { display (result.data); }
            else                                                { display (); }
        } else { display (result.resultType); }
    }

    public void onLoaderReset (Loader <LoadResult <List <Movie>>> loader) {
        display ();
    }

    private void display () {
        log.setText (R.string.load_failed);
        log.setVisibility (View.VISIBLE);

        progress.setVisibility (View.GONE);
        recycler.setVisibility (View.GONE);
    }

    private void display (List <Movie> list) {
        if (adapter == null) {
            adapter = new RecyclerAdapter (this);
            recycler.setAdapter    (adapter);
        }

        adapter.addMovies (list);
        recycler.setVisibility (View.VISIBLE);

        log.setVisibility      (View.GONE);
        progress.setVisibility (View.GONE);
    }

    private void display (ResultType result) {
        if (result == ResultType.NO_INTERNET) { log.setText (R.string.no_internet); }
        else                                  { log.setText (R.string.load_failed); }
                                                log.setVisibility (View.VISIBLE);

        progress.setVisibility (View.GONE);
        recycler.setVisibility (View.GONE);
    }

}
