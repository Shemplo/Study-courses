package me.shemplo.smsreceiver;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;

public class StubScreen extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate (savedInstanceState);
        setContentView (R.layout.activity_stub_screen);

        System.out.println ("DEBUG");
    }
}
