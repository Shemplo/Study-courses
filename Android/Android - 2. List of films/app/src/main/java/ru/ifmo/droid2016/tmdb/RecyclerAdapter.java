package ru.ifmo.droid2016.tmdb;

import android.content.Context;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.facebook.drawee.view.SimpleDraweeView;

import java.util.Collections;
import java.util.List;

import ru.ifmo.droid2016.tmdb.model.Movie;

/**
 * Created by Андрей on 21.11.2016.
 */

public class RecyclerAdapter extends RecyclerView.Adapter <RecyclerAdapter.MovieViewHolder> {

    private Context context;
    private LayoutInflater inflater;

    private List<Movie> list = Collections.emptyList ();

    public RecyclerAdapter (Context context) {
        this.context  = context;
        this.inflater = LayoutInflater.from (this.context);
    }

    public void addMovies (List <Movie> list) {
        this.list = list;
        notifyDataSetChanged ();
    }

    public void onBindViewHolder (MovieViewHolder holder, int position) {
        Movie movie   = list.get (position);

        String poster = movie.posterPath;
        holder.poster.setImageURI (poster);

        holder.title.setText (movie.localizedTitle);
        if (movie.localizedTitle.length() == 0) { holder.title.setText (movie.originalTitle); }
        holder.overview.setText (movie.overviewText);

        holder.rating.setText (R.string.rating);
        holder.rating.setText (holder.rating.getText () + ": " + movie.voteRange);

        holder.release.setText (R.string.release);
        holder.release.setText (holder.release.getText () + ": " + movie.releaseDate);
    }

    public MovieViewHolder onCreateViewHolder (ViewGroup parent, int viewType) {
        return MovieViewHolder.create (inflater, parent);
    }

    public int getItemCount () {
        return list.size ();
    }

    static class MovieViewHolder extends RecyclerView.ViewHolder {

        SimpleDraweeView poster;
        TextView         title;
        TextView         overview;
        TextView         rating;
        TextView         release;

        private MovieViewHolder (View item) {
            super (item);

            poster    = (SimpleDraweeView) item.findViewById (R.id.posterView);
            title     = (TextView)         item.findViewById (R.id.titleView);
            overview  = (TextView)         item.findViewById (R.id.overviewView);
            rating    = (TextView)         item.findViewById (R.id.ratingView);
            release   = (TextView)         item.findViewById (R.id.releaseView);
        }

        static MovieViewHolder create (LayoutInflater infalter, ViewGroup parent) {
            return new MovieViewHolder (infalter.inflate (R.layout.movie_unit, parent, false));
        }

    }

}
