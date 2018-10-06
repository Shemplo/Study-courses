package ru.shemplo.shop.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class DBAccess implements AutoCloseable {

	private static DBAccess instance;
	
	public static DBAccess getInstanceOf (DBType type) {
		if (instance == null) {
			try {
				instance = new DBAccess (type);
			} catch (SQLException sqle) {
				System.err.println (sqle);
			}
		}
		
		return instance;
	}
	
	private final Connection DB;
	
	private DBAccess (final DBType type) throws SQLException {
		String dbUrl = System.getProperty ("shop.db.url");
		if (dbUrl == null || dbUrl.length () == 0) {
			String text = "Property `shop.db.url` has invalid value (EMPTY)";
			throw new IllegalStateException (text);
		}
		
		String url = String.join (":", "jdbc", type.TYPE, dbUrl);
		this.DB = DriverManager.getConnection (url);
	}

	@Override
	public void close () throws Exception {
		if (this.DB != null) {
			this.DB.close ();
		}
	}
	
}
