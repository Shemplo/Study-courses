package ru.shemplo.shop;

import static org.eclipse.jetty.servlet.ServletContextHandler.*;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.Servlet;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

import ru.shemplo.shop.db.DBAccess;
import ru.shemplo.shop.db.DBLib;
import ru.shemplo.shop.servlet.AddProductServlet;
import ru.shemplo.shop.servlet.GetProductsServlet;
import ru.shemplo.shop.servlet.QueryServlet;
import ru.shemplo.snowball.stuctures.Pair;
import ru.shemplo.snowball.utils.db.DBType;

public class Run {
	
	static {
		PropertiesLoader.load ("src/main/resources/properties");
	}
	
	private static List <Pair <String, Servlet>> SERVLETS = new ArrayList <> ();
	static {
		SERVLETS.add (Pair.mp ("/add-product" , new AddProductServlet  ()));
		SERVLETS.add (Pair.mp ("/get-products", new GetProductsServlet ()));
		SERVLETS.add (Pair.mp ("/query"       , new QueryServlet       ()));
	}
	
	public static void main (String ... args) throws Exception {
		DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
		db.update (DBLib.createTable ());
		
		Server server = new Server (8081);
		ServletContextHandler context = new ServletContextHandler (SESSIONS);
		
		SERVLETS.forEach (p -> context.addServlet (new ServletHolder (p.S), p.F));
        context.setContextPath ("/");
        server.setHandler (context);
        
        server.start ();
        server.join ();
		
		db.close ();
	}
	
}
