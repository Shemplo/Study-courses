package ru.shemplo.shop.servlet;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import ru.shemplo.shop.db.DBAccess;
import ru.shemplo.shop.db.DBLib;
import ru.shemplo.shop.servlet.html.HTMLBuilder;
import ru.shemplo.snowball.utils.db.DBType;

public class GetProductsServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = -500173686782666575L;
	
	@Override
	protected void doGet (HttpServletRequest req, HttpServletResponse resp) 
			throws ServletException, IOException {
		HTMLBuilder html = new HTMLBuilder ("Query page");
		resp.setContentType ("text/html");
		
		try {
			DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
			
			String query = DBLib.selectOrderedColumn ("name", "ASC", false);
			ResultSet result = db.execute (query);
			
			html.addSmallHeader ("Products in database: ");
			HTMLBuilder.resultToHTML (result, html, "name", "price");
		} catch (SQLException sqle) {
			html.addHeader ("Processing query failed");
			html.addParagraph ("<b>Error:</b> " + sqle);
			
			resp.setStatus (HttpServletResponse.SC_NOT_ACCEPTABLE);
			resp.getWriter ().println (html);
			
			return;
		}
		
		resp.setStatus (HttpServletResponse.SC_OK);
		resp.getWriter ().println (html);
	}

}
