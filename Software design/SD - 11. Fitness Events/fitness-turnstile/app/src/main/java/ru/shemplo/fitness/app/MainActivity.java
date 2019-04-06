package ru.shemplo.fitness.app;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.Intent;
import android.content.IntentFilter;
import android.nfc.NfcAdapter;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.util.Base64;
import android.util.Log;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;

import java.io.IOException;
import java.lang.ref.WeakReference;

import ru.shemplo.fitness.app.services.TicketService;

public class MainActivity extends Activity {

    private static final String TAG = MainActivity.class.getName();

    private TextView nfcText;
    private TextView passText;
    private TextView failText;
    private View loading;

    private NfcAdapter nfcAdapter;
    private PendingIntent nfcPendingIntent;

    private boolean isProcessing = false;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        nfcAdapter = NfcAdapter.getDefaultAdapter(this);

        if (nfcAdapter == null) {
            Toast.makeText(this, R.string.nfc_not_supported, Toast.LENGTH_LONG).show();
            finish();
            return;
        }

        nfcText = findViewById(R.id.nfc);
        passText = findViewById(R.id.pass);
        failText = findViewById(R.id.fail);
        loading = findViewById(R.id.loading);

        if (!nfcAdapter.isEnabled()) {
            nfcText.setText(R.string.nfc_disabled);
        }

        nfcPendingIntent = PendingIntent.getActivity(this, 0, new Intent(this, this.getClass()).addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP), 0);
    }

    public void showNfc() {
        nfcText.setVisibility(View.VISIBLE);
        passText.setVisibility(View.GONE);
        failText.setVisibility(View.GONE);
        loading.setVisibility(View.GONE);
    }

    public void showPass() {
        nfcText.setVisibility(View.GONE);
        passText.setVisibility(View.VISIBLE);
        failText.setVisibility(View.GONE);
        loading.setVisibility(View.GONE);
    }

    public void showFail() {
        nfcText.setVisibility(View.GONE);
        passText.setVisibility(View.GONE);
        failText.setVisibility(View.VISIBLE);
        loading.setVisibility(View.GONE);
    }

    public void showLoading() {
        nfcText.setVisibility(View.GONE);
        passText.setVisibility(View.GONE);
        failText.setVisibility(View.GONE);
        loading.setVisibility(View.VISIBLE);
    }

    public void showToast(String message) {
        Toast.makeText(this, message, Toast.LENGTH_LONG).show();
    }

    private void processPass(String id) {
        Log.d(TAG, "Pass ID: [" + id + "]");
        new DatabaseRequestTask(this).execute(id);
    }

    @Override
    public void onNewIntent(Intent intent) {
        String action = intent.getAction();

        Log.d(TAG, "Got intent: " + action);

        if (action == null || isProcessing) return;

        switch (action) {
            case NfcAdapter.ACTION_TAG_DISCOVERED:
            case NfcAdapter.ACTION_NDEF_DISCOVERED:
            case NfcAdapter.ACTION_TECH_DISCOVERED:
                byte[] extraIdBytes = intent.getByteArrayExtra(NfcAdapter.EXTRA_ID);
                if (extraIdBytes != null) {
                    String extraId = Base64.encodeToString(extraIdBytes, Base64.DEFAULT).trim();
                    processPass(extraId);
                }
            default:
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        setNFCForegroundMode(true);
    }

    @Override
    protected void onPause() {
        setNFCForegroundMode(false);
        super.onPause();
    }

    private void setNFCForegroundMode(boolean enabled) {
        if (nfcAdapter == null || !nfcAdapter.isEnabled()) return;

        if (enabled) {
            Log.d(TAG, "Enable NFC Foreground Mode");

            IntentFilter tagDetected = new IntentFilter(NfcAdapter.ACTION_TAG_DISCOVERED);
            IntentFilter[] writeTagFilters = new IntentFilter[]{tagDetected};
            nfcAdapter.enableForegroundDispatch(this, nfcPendingIntent, writeTagFilters, null);
        } else {
            Log.d(TAG, "Disable NFC Foreground Mode");

            nfcAdapter.disableForegroundDispatch(this);
        }
    }

    private static class DatabaseRequestTask extends AsyncTask<String, Integer, Integer> {

        private WeakReference<MainActivity> contextReference;

        DatabaseRequestTask(MainActivity context) {
            this.contextReference = new WeakReference<>(context);
        }

        @Override
        protected Integer doInBackground(String... args) {
            try {
                String id = args[0];

                TicketService service = new TicketService();
                int visits = service.getVisitOfTicketBySecret(id);
                Log.d(TAG, "Visits: " + visits);
                return visits;

            } catch (IOException e) {
                e.printStackTrace();
                cancel(true);
            }
            return -1;
        }

        @Override
        protected void onPreExecute() {
            MainActivity context = contextReference.get();
            if (context == null) return;

            context.isProcessing = true;
            context.showLoading();
        }

        @Override
        protected void onPostExecute(Integer result) {
            final MainActivity context = contextReference.get();
            if (context == null) return;

            context.isProcessing = false;
            context.showToast("Visits left: " + (result - 1));
            if (result > 0) {
                context.showPass();
            } else {
                context.showFail();
            }

            new Handler().postDelayed(new Runnable() {

                @Override
                public void run() {
                    context.showNfc();
                }
            }, 3000);
        }

        @Override
        protected void onCancelled() {
            MainActivity context = contextReference.get();
            if (context == null) return;

            context.isProcessing = false;
            context.showNfc();
        }
    }
}


