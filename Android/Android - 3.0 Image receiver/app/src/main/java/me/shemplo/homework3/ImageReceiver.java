package me.shemplo.homework3;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.telephony.SmsMessage;
import android.util.Log;

/**
 * Created by Андрей on 29.11.2016.
 */

public class ImageReceiver extends BroadcastReceiver {

    private static final String LISTEN = "android.provider.Telephony.SMS_RECEIVED";

    public void onReceive (Context context, Intent intent) {
        Log.d ("SMSR", "Smth happend");

        if (intent.getAction() != null
                && intent.getAction().equalsIgnoreCase(LISTEN)) {
            Log.d ("SMSR", "Received event");

            Object [] pduArray = (Object []) intent.getExtras ().get ("pdus");
            SmsMessage [] messages = new SmsMessage [pduArray.length];

            for (int i = 0; i < pduArray.length; i ++) {
                messages [i] = SmsMessage.createFromPdu ((byte []) pduArray [i]);
                Log.d ("SMSR", messages [i].getDisplayMessageBody());
            }

            StringBuilder message = new StringBuilder ();
            for (int i = 0; i < messages.length; i ++) {
                message.append (messages [i].getMessageBody ().trim () + " ");
            }

            String body = message.toString ();

            Intent service = new Intent (context, ImageService.class);
            service.putExtra ("sms", body.trim ());
            context.startService (service);
        }
    }

}
