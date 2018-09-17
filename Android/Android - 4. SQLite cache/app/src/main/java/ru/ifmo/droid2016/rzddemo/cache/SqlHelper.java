package ru.ifmo.droid2016.rzddemo.cache;

import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.database.sqlite.SQLiteStatement;
import android.icu.text.SimpleDateFormat;
import android.provider.BaseColumns;
import android.text.TextUtils;
import android.util.Log;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Locale;

import ru.ifmo.droid2016.rzddemo.model.TimetableEntry;
import ru.ifmo.droid2016.rzddemo.utils.TimeUtils;

/**
 * Created by Андрей on 08.12.2016.
 */

public class SqlHelper extends SQLiteOpenHelper {

    private static SqlHelper instance;

    @DataSchemeVersion
    private final int version;

    private SimpleDateFormat dateFormat = new SimpleDateFormat ("yyyy-MM-dd", Locale.getDefault ());
    private SimpleDateFormat timeFormat = new SimpleDateFormat ("yyyy-MM-dd'T'HH:mm", Locale.getDefault ());

    private SqlHelper (Context context, @DataSchemeVersion int version) {
        super(context, Strings.DATABASE, null, version);
        this.version = version;
    }

    public void onCreate (SQLiteDatabase database) {
        String query = ""
                + "CREATE TABLE " + Strings.Timetable.TABLE + " ("
                + TextUtils.join (" TEXT, ", version == DataSchemeVersion.V2
                                                ? Strings.Timetable.formVersion2
                                                : Strings.Timetable.formVersion1)
                + " TEXT)";
        database.execSQL (query);
    }

    public void onUpgrade (SQLiteDatabase database, int oldV, int newV) {
        String query = ""
                + "ALTER TABLE " + Strings.Timetable.TABLE + " "
                + "ADD COLUMN " + Strings.Timetable.TRAIN_NAME + " TEXT NULL";
        database.execSQL (query);
    }

    public void onDowngrade (SQLiteDatabase database, int oldV, int newV) {
        String tableName = Strings.Timetable.TABLE + "_downgrade";
        String query = ""
                + "ALTER TABLE " + Strings.Timetable.TABLE + " "
                + "RENAME TO " + tableName;
        String cols = TextUtils.join (",", version == DataSchemeVersion.V2
                                            ? Strings.Timetable.formVersion2
                                            : Strings.Timetable.formVersion1);

        database.execSQL (query);
        onCreate (database);

        query = ""
                + "INSERT INTO " + Strings.Timetable.TABLE + " (" + cols + ") "
                + "SELECT " + cols + " FROM " + tableName;
        database.execSQL (query);

        query = ""
                + "DROP TABLE " + tableName;
        database.execSQL (query);
    }

    private Calendar parseCalendar (String value) throws ParseException {
        Calendar calendar = Calendar.getInstance ();
        calendar.setTimeZone (TimeUtils.getMskTimeZone ());
        calendar.setTime (timeFormat.parse (value));

        return calendar;
    }

    public List<TimetableEntry> getTimetable (String from, String to, Calendar calendar) {
        SQLiteDatabase database = getReadableDatabase ();
        List <TimetableEntry> answer = new ArrayList <> ();

        Cursor cursor = null;

        try {
            cursor = database.query (Strings.Timetable.TABLE,
                                     version == DataSchemeVersion.V2
                                                ? Strings.Timetable.formVersion2
                                                : Strings.Timetable.formVersion1,
                                     Strings.Timetable.DATE + "=? AND "
                                             + Strings.Timetable.DEPARTURE_STATION_ID + "=? AND "
                                             + Strings.Timetable.ARRIVAL_STATION_ID + "=?",
                                     new String [] {
                                        dateFormat.format (calendar.getTime ()),
                                        from,
                                        to
                                     },
                                     null,
                                     null,
                                     null);
            if (cursor != null && cursor.moveToFirst ()) {
                for (; !cursor.isAfterLast (); cursor.moveToNext ()) {
                    int key = 1;

                    TimetableEntry entry = new TimetableEntry (
                            cursor.getString (key ++),
                            cursor.getString (key ++),
                            parseCalendar (cursor.getString (key ++)),
                            cursor.getString (key ++),
                            cursor.getString (key ++),
                            parseCalendar (cursor.getString (key ++)),
                            cursor.getString (key ++),
                            (version == DataSchemeVersion.V2 ? cursor.getString (key ++) : null),
                            cursor.getString (key ++),
                            cursor.getString (key ++)
                    );

                    answer.add (entry);
                }
            }
        } catch (Exception e) {
            e.printStackTrace ();
        } finally {
            if (cursor != null) { cursor.close(); }
        }

        return answer;
    }

    public void putTimetable (Calendar calendar, List <TimetableEntry> timetable) {
        String query = ""
                + "INSERT INTO " + Strings.Timetable.TABLE + " ("
                + TextUtils.join (", ", version == DataSchemeVersion.V2
                                                    ? Strings.Timetable.formVersion2
                                                    : Strings.Timetable.formVersion1)
                + ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?"
                + (version == DataSchemeVersion.V2 ? ", ?" : "") + ")";

        SQLiteDatabase database = getWritableDatabase ();

        try {
            SQLiteStatement statement = database.compileStatement (query);
            database.beginTransaction ();

            for (TimetableEntry entry: timetable) {
                int key = 1;

                statement.bindString(key ++, dateFormat.format (calendar.getTime()));
                statement.bindString(key ++, entry.departureStationId);
                statement.bindString(key ++, entry.departureStationName);
                statement.bindString(key ++, timeFormat.format (entry.departureTime.getTime ()));
                statement.bindString(key ++, entry.arrivalStationId);
                statement.bindString(key ++, entry.arrivalStationName);
                statement.bindString(key ++, timeFormat.format (entry.arrivalTime.getTime ()));
                statement.bindString(key ++, entry.trainRouteId);
                if (version == DataSchemeVersion.V2) {
                    if (entry.trainName == null) { statement.bindNull (key++); }
                    else { statement.bindString (key++, entry.trainName); }
                }
                statement.bindString (key ++, entry.routeStartStationName);
                statement.bindString (key, entry.routeEndStationName);

                statement.executeInsert ();
                statement.clearBindings ();
            }

            database.setTransactionSuccessful ();
        } catch (Exception e) {
            //Smth should be written here :(
            Log.d ("SH", "Everything is bad :(");
            e.printStackTrace ();
        } finally {
            database.endTransaction ();
        }
    }

    public static SqlHelper getInstance (Context context, @DataSchemeVersion int version) {
        if (instance == null) {
            synchronized (SqlHelper.class) {
                instance = instance == null
                            ? new SqlHelper (context.getApplicationContext (), version)
                            : instance;
            }
        }

        return instance;
    }

    private static final class Strings {

        public static final String DATABASE = "rzd.db";

        static abstract class Timetable implements BaseColumns {

            public static final String TABLE = "timetable";

            private static final String DATE = "date";
            private static final String TRAIN_NAME               = "train_name";

            private static final String ARRIVAL_STATION_ID       = "arrival_station_id";
            private static final String ARRIVAL_STATION_NAME     = "arrival_station_name";
            private static final String ARRIVAL_TIME             = "arrival_time";

            private static final String DEPARTURE_STATION_ID     = "departure_station_id";
            private static final String DEPARTURE_STATION_NAME   = "departure_station_name";
            private static final String DEPARTURE_TIME           = "departure_time";

            private static final String TRAIN_ROUTE_ID           = "train_route_id";
            private static final String ROUTE_START_STATION_NAME = "route_start_station_name";
            private static final String ROUTE_END_STATION_NAME   = "route_end_station_name";

            private static final String [] formVersion1 = {
                    DATE,
                    DEPARTURE_STATION_ID, DEPARTURE_STATION_NAME, DEPARTURE_TIME,
                    ARRIVAL_STATION_ID, ARRIVAL_STATION_NAME, ARRIVAL_TIME,
                    TRAIN_ROUTE_ID,
                    ROUTE_START_STATION_NAME,
                    ROUTE_END_STATION_NAME
            };

            private static final String [] formVersion2 = {
                    DATE,
                    DEPARTURE_STATION_ID, DEPARTURE_STATION_NAME, DEPARTURE_TIME,
                    ARRIVAL_STATION_ID, ARRIVAL_STATION_NAME, ARRIVAL_TIME,
                    TRAIN_ROUTE_ID, TRAIN_NAME,
                    ROUTE_START_STATION_NAME,
                    ROUTE_END_STATION_NAME
            };

        }

    }

}
