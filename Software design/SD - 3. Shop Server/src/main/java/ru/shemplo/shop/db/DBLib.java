package ru.shemplo.shop.db;

public class DBLib {

	public static String createTable () {
		return new StringBuilder ()
			 . append ("CREATE TABLE IF NOT EXISTS `products` (")
			 . append ("`id`    INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,")
			 . append ("`name`  TEXT NOT NULL,")
			 . append ("`price` INT  NOT NULL")
			 . append (")")
			 . toString ();
	}
	
	public static String insertProduct (String name, long price) {
		return new StringBuilder ()
			 . append ("INSERT ")
			 . append ("INTO `products` (`name`, `price`) ")
			 . append ("VALUES ('").append (name)
			 . append ("', '").append (price).append ("')")
			 . toString ();
	}
	
	public static String selectOrderedColumn (String colun, String order, boolean limited) {
		return new StringBuilder ()
			 . append ("SELECT * ")
			 . append ("FROM `products` ")
			 . append ("ORDER BY `").append (colun).append ("` ")
			 . append (order).append (" ")
			 . append (limited ? "LIMIT 1" : "")
			 . toString ();
	}
	
	public static String selectAggregation (String function, String column) {
		return new StringBuilder ()
			 . append ("SELECT ").append (function)
			 . append ("(").append (column).append (") AS 'result' ")
			 . append ("FROM `products`")
			 . toString ();
	}
	
}
