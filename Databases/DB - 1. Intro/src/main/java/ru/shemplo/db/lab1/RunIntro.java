package ru.shemplo.db.lab1;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;

import java.util.Arrays;
import java.util.Random;
import java.util.StringJoiner;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class RunIntro {
    
    private static String HOST, USER, PASSWORD, DB;
    
    static {
        try (
            InputStream is = new FileInputStream ("db-pass");
            Reader r = new InputStreamReader (is, StandardCharsets.UTF_8);
            BufferedReader br = new BufferedReader (r);
        ) {
            HOST     = br.readLine ().trim ();
            USER     = br.readLine ().trim ();
            PASSWORD = br.readLine ().trim ();
            DB       = br.readLine ().trim ();
        } catch (IOException ioe) {
            System.err.println (ioe);
        }
    }
    
    private static final String URL = "jdbc:postgresql://" + HOST + ":5432/" + DB;
    private static final Random RANDOM = new Random ();
    
    private static final Supplier <String> GET_STRING = () -> {
        StringBuilder sb = new StringBuilder ();
        Stream.iterate (0, i -> i + 1).limit (16)
              .map (__ -> RANDOM.nextInt ('z' - 'a' + 1))
              .map (vv -> new Character ((char) ('a' + vv)))
              .forEach (sb::append);
        return sb.toString ();
    };
    
    private static enum Type {
        INT, REAL, TEXT, CHAR
    }
    
    private static final Supplier <Type> GET_TYPE = () -> {
        Type [] values = Type.values ();
        return values [RANDOM.nextInt (values.length)];
    };
    
    public static void main (String ... args) throws Exception {
        Connection connection = DriverManager.getConnection (URL, USER, PASSWORD);
        final String BASE_CREATE_QUERY = "CREATE TABLE IF NOT EXISTS :tname (:columns)",
                     BASE_INSERT_QUERY = "INSERT INTO :tname (:names) VALUES (:values)";
        for (int i = 0; i < 10; i++) {
            final int columns = 1 + RANDOM.nextInt (5);
            StringJoiner sj = new StringJoiner (", ");
            String [] names = new String [columns];
            Type [] model = new Type [columns];
            
            sj.add ("id SERIAL PRIMARY KEY");
            for (int j = 0; j < columns; j++) {
                StringBuilder sb = new StringBuilder ();
                
                sb.append (names [j] = GET_STRING.get ()).append (" ")
                  .append ((model [j] = GET_TYPE.get ()) == Type.CHAR 
                          ? "CHAR(50)" : model [j].name ())
                  .append (RANDOM.nextBoolean () ? " NOT NULL" : "");
                sj.add (sb);
            }
            
            final String tableName = GET_STRING.get ();
            String query = BASE_CREATE_QUERY
                         . replace (":tname", tableName)
                         . replace (":columns", sj.toString ());
            Statement s = connection.createStatement ();
            s.executeUpdate (query);
            
            final int rows = RANDOM.nextInt (40);
            for (int j = 0; j < rows; j++) {
                sj = new StringJoiner (", ");
                Arrays.asList (names).forEach (sj::add);
                String columnsNames = sj.toString ();
                
                sj = new StringJoiner (", ");
                for (int k = 0; k < model.length; k++) {
                    switch (model [k]) {
                        case INT : sj.add ("" + RANDOM.nextInt ());        break;
                        case REAL: sj.add ("" + RANDOM.nextFloat ());      break;
                        default  : sj.add ("'" + GET_STRING.get () + "'"); break;
                    }
                }
                
                query = BASE_INSERT_QUERY
                      . replace (":tname", tableName)
                      . replace (":names", columnsNames)
                      . replace (":values", sj.toString ());
                s = connection.createStatement ();
                s.executeUpdate (query);
            }
        }
        connection.close ();
    }
    
}
