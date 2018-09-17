package me.shemplo.homework3;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import java.io.File;
import java.io.FileInputStream;

public class MainActivity extends AppCompatActivity {

    public static final String IMAGE = "image.jpg";

    private ImageView image;
    private TextView  console;
    private BroadcastReceiver imageReceiver;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate (savedInstanceState);
        setContentView (R.layout.activity_main);

        image   = (ImageView) findViewById (R.id.image);
        console = (TextView)  findViewById (R.id.console);

        //Try to draw if file downloaded
        display ();

        imageReceiver = new BroadcastReceiver() {

            public void onReceive (Context context, Intent intent) {
                //Get request from service to display image
                display ();
            }

        };

        //Bind receiver to listen to the service
        registerReceiver (imageReceiver, new IntentFilter ("ImageLoader"));
    }

    public void onDestroy () {
        super.onDestroy ();

        //Unbind receiver from events
        unregisterReceiver (imageReceiver);
    }

    private void downloading () {
        console.setText (R.string.log_downloading);

        console.setVisibility (View.VISIBLE);
        image.setVisibility   (View.INVISIBLE);
    }

    private void display () {
        //Getting file
        File main = new File (getFilesDir (), IMAGE);

        boolean showImage = true;
        boolean showError = false;
        console.setText (R.string.error_loading_failed);

        if (main.exists ()) {
            //File successfully downloaded

            try {
                FileInputStream input = new FileInputStream (main);
                Bitmap value = BitmapFactory.decodeStream (input);

                //Copying data from file to ImageView
                image.setImageBitmap (value);
            } catch (Exception e) {
                Log.d ("MainActivity", "Failed to read data from file");

                showImage = false;
                showError = true;
            }
        } else {


            showImage = false;
            showError = true;
        }

        console.setVisibility (showError ? View.VISIBLE : View.INVISIBLE);
        image.setVisibility   (showImage ? View.VISIBLE : View.INVISIBLE);
    }

}
