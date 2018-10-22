package ru.shemplo.tasks.db;

import static java.sql.DriverManager.*;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLNonTransientConnectionException;

import ru.shemplo.snowball.utils.db.DBType;

public class DBConnection implements AutoCloseable {

	private volatile static DBConnection instance;
	
	public static DBConnection getInstanceOf (DBType type) {
		if (instance == null) {
			try {
				instance = new DBConnection (type);
			} catch (SQLException sqle) {
				System.err.println (sqle);
			}
		}
		
		return instance;
	}
	
	public static DBConnection asInstanceOf (DBConnection db) {
	    if (instance != null) {
	        try   { instance.close (); } 
	        catch (Exception e) {}
	    }
	    
	    if (instance == null) {
	        instance = db;
	    }
	    
	    return instance;
	}
	
	private final DBType type;
	private Connection DB;
	
	DBConnection (final DBType type) throws SQLException {
		this.type = type; this.DB = connect ();
	}
	
	DBConnection () throws SQLException {
	    this.type = null; this.DB = null;
	}
	
	private final Connection connect () throws SQLException {
	    String dbUrl = System.getProperty ("tasks.db.url");
        if (dbUrl == null || dbUrl.length () == 0) {
            String text = "Property `tasks.db.url` has invalid value (EMPTY)";
            throw new IllegalStateException (text);
        }
        
        String login    = System.getProperty ("tasks.db.login"),
               password = System.getProperty ("tasks.db.password");
        
        String url = String.join (":", "jdbc", type.TYPE, dbUrl);
        if (login != null && password != null) {
            return getConnection (url, login, password);
        }
        
        return getConnection (url);
	}
	
	
	public ResultSet execute (String query) throws SQLException {
	    try {
	        return DB.prepareStatement (query).executeQuery ();
	    } catch (SQLNonTransientConnectionException sqlntce) {
	        // Try to reconnect to database
	        this.DB = connect ();
	    }
	    
	    return DB.prepareStatement (query).executeQuery ();
	}
	
	public void update (String query) throws SQLException {
	    try {
	        DB.prepareStatement (query).execute ();
        } catch (SQLNonTransientConnectionException sqlntce) {
            // Try to reconnect to database
            this.DB = connect ();
        }
	    
		DB.prepareStatement (query).execute ();
	}

	@Override
	public void close () throws Exception {
		if (this.DB != null) {
			this.DB.close ();
			instance = null;
		}
	}
	
}
