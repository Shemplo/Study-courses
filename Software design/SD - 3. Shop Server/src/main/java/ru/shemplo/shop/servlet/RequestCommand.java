package ru.shemplo.shop.servlet;

import java.util.function.Consumer;

import java.sql.ResultSet;
import java.sql.SQLException;

import ru.shemplo.shop.db.DBAccess;
import ru.shemplo.shop.db.DBLib;
import ru.shemplo.shop.servlet.html.HTMLBuilder;
import ru.shemplo.snowball.stuctures.Pair;

public enum RequestCommand {

	MAX   (pr -> orderOperaion (pr, "DESC", "max")),
	MIN   (pr -> orderOperaion (pr, "ASC", "min")),
	SUM   (pr -> aggregateOperation (pr, "SUM", "price")),
	COUNT (pr -> aggregateOperation (pr, "COUNT", "*"));
	
	private static void orderOperaion (Pair <DBAccess, HTMLBuilder> pr, String order, String minMax) {
		try {
			String query = DBLib.selectOrderedColumn ("price", order, true);
			ResultSet result = pr.F.execute (query);
			
			pr.S.addSmallHeader ("Product with " + minMax + " price: ");
			HTMLBuilder.resultToHTML (result, pr.S, "name", "price");
		} catch (SQLException | IllegalStateException sqle) {
			throw new IllegalStateException (sqle);
		}
	}
	
	private static void aggregateOperation (Pair <DBAccess, HTMLBuilder> pr, 
			String function, String column) {
		try {
			String query = DBLib.selectAggregation (function, column);
			ResultSet result = pr.F.execute (query);
			
			if (result.next ()) {
				pr.S.addParagraph ("Result of function " + function 
				  + " = " + result.getString ("result"));
			}
		} catch (SQLException | IllegalStateException sqle) {
			throw new IllegalStateException (sqle);
		}
	}
	
	public final Consumer <Pair <DBAccess, HTMLBuilder>> C;
	
	private RequestCommand (Consumer <Pair <DBAccess, HTMLBuilder>> funtion) {
		this.C = funtion;
	}
	
}
