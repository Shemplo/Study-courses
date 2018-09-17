package me.shemplo.smsreceiver;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

/**
 * Created by Андрей on 15.11.2016.
 */

public class SmsService extends Service {

    public IBinder onBind (Intent intent) {
        return null;
    }

    public int onStartCommand (Intent intent, int flags, int startID) {
        String body = intent.getExtras ().getString ("smsBody");
        System.out.println (body);

        return START_STICKY;
    }

}
