package ru.shemplo.fitness.db;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;

import java.util.*;

import ru.shemplo.fitness.AppConfiguration;
import ru.shemplo.fitness.entities.Database;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class DefaultDBManager implements DBUpdateManager, DBRetriveManager, DBManager {
    
    private DBObjectUnwrapper objectUnwrapper;
    private AppConfiguration configuration;
    
    @Override
    public void update (String ... requests) throws IOException, SQLException {
        try (final Connection connection = openConnection ()) {
            for (String request : requests) {
                connection.prepareStatement (request)
                          .execute          ();
            }
        }
    }
    
    @Override
    public <T> List <T> retrieve (String request, Class <T> type) 
            throws IOException, SQLException {
        List <T> result = new ArrayList <> ();
        
        try (
            Connection connection = openConnection ();
        ) {
            ResultSet set = connection.prepareStatement (request).executeQuery ();
            Map <String, Object> map = new HashMap <> ();
            while (set.next ()) {
                int columns = set.getMetaData ().getColumnCount ();
                for (int i = 0; i < columns; i++) {
                    String name  = set.getMetaData ().getColumnName (i + 1);
                    name = name.toLowerCase ().replace ("_", "");
                    Object value = set.getObject (i + 1);
                    map.put (name, value);
                }
                
                System.out.println (map);
                result.add (objectUnwrapper.unwrap (map, type));
            }
        }
        
        return result;
    }
    
    @Override
    public <T> T runFunction (String request) throws IOException, SQLException {
        try (
            Connection connection = openConnection ();
        ) {
            ResultSet set = connection.prepareStatement (request).executeQuery ();
            set.next (); // Fetching result of function
            
            @SuppressWarnings ("unchecked") T result = (T) set.getObject (1);
            return result;
        }
    }
    
    protected Connection openConnection () throws IOException, SQLException {
        Database db = configuration.<Database> get ("database-profile").get ();
        if (db == null) {
            String message = "Configuration for connecting to DB is not found";
            throw new IOException (message);
        }
        
        final String password = Optional.ofNullable (db.getPassword ()).orElse ("");
        return DriverManager.getConnection (db.getURL (), db.getLogin (), password);
    }
    
}
