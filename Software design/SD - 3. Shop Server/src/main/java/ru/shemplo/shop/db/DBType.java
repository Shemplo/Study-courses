package ru.shemplo.shop.db;

public enum DBType {

	SQLite ("sqlite")
	;
	
	public final String TYPE;
	
	private DBType (String type) {
		this.TYPE = type;
	}
	
}
