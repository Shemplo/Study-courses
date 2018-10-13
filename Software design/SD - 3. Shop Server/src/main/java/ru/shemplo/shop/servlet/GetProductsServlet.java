package ru.shemplo.shop.servlet;

import static ru.shemplo.shop.servlet.html.HTMLBuilder.*;

import java.util.Map;

import java.sql.ResultSet;

import ru.shemplo.shop.db.DBLib;
import ru.shemplo.shop.servlet.html.HTMLBuilder;

public class GetProductsServlet extends AbsServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = -500173686782666575L;
	
	public GetProductsServlet () {
		super ("List of products", new String [] {});
	}

	@Override
	public void computeRequest (Map <String, String []> params, HTMLBuilder html) 
	        throws Exception {
	    String query = DBLib.selectOrderedColumn ("name", "ASC", false);
        ResultSet result = DB.execute (query);
        
        html.addSmallHeader ("Products in database: ");
        resultToHTML (result, html, "name", "price");
	}

}
