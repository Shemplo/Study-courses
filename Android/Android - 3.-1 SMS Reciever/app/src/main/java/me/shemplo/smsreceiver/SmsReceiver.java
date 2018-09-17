package me.shemplo.smsreceiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.telephony.SmsMessage;

import java.io.Console;

/**
 * Created by Андрей on 15.11.2016.
 */

public class SmsReceiver extends BroadcastReceiver {

    private static final String ACTION = "android.provider.Telephony.SMS_RECEIVED";

    public void onReceive (Context context, Intent intent) {

        if (intent != null && intent.getAction () != null &&
                ACTION.compareToIgnoreCase (intent.getAction ()) == 0) {

            Object [] pduArray = (Object []) intent.getExtras ().get ("pdus");
            SmsMessage [] messages = new SmsMessage [pduArray.length];

            for (int i = 0; i < pduArray.length; i ++) {

                messages [i] = SmsMessage.createFromPdu ((byte []) pduArray [i]);
            }

            String smsFrom = messages [0].getDisplayOriginatingAddress ();
            System.out.println (smsFrom);

            StringBuilder sb = new StringBuilder ();
            for (int i = 0; i < messages.length; i ++) {
                sb.append (messages [i].getMessageBody ());
            }

            String body = sb.toString ();

            Intent service = new Intent (context, SmsService.class);
            service.putExtra ("smsBody", body);
            context.startService (service);
        }
    }

}
