package ru.shemplo.shop.servlet;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import ru.shemplo.shop.db.DBAccess;
import ru.shemplo.shop.servlet.html.HTMLBuilder;
import ru.shemplo.snowball.stuctures.Pair;
import ru.shemplo.snowball.utils.db.DBType;

public class QueryServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4442168039401537019L;

	@Override
	protected void doGet (HttpServletRequest req, HttpServletResponse resp) 
			throws ServletException, IOException {
		HTMLBuilder html = new HTMLBuilder ("Query page");
		resp.setContentType ("text/html");
		
		String command = req.getParameter ("command");
		if (command == null || command.length () == 0) {
			html.addSmallHeader ("Processing query failed");
			html.addParagraph ("<b>Error:</b> empty `command` parameter");
			
			resp.setStatus (HttpServletResponse.SC_BAD_REQUEST);
			resp.getWriter ().println (html);
			
			return;
		}
		
		RequestCommand en = null;
		try {
			en = RequestCommand.valueOf (command.toUpperCase ());
		} catch (IllegalArgumentException iae) {
			html.addSmallHeader ("Processing query failed");
			html.addParagraph ("<b>Error:</b> unknown command `" + command + "`");
			
			resp.setStatus (HttpServletResponse.SC_BAD_REQUEST);
			resp.getWriter ().println (html);
			
			return;
		}
		
		try {
			DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
			// Processing function from request
			en.C.accept (Pair.mp (db, html));
		} catch (IllegalStateException ise) {
			html.addSmallHeader ("Processing query failed");
			html.addParagraph ("<b>Error:</b> " + ise);
			
			resp.setStatus (HttpServletResponse.SC_NOT_ACCEPTABLE);
			resp.getWriter ().println (html);
			
			return;
		}
		
		resp.setStatus (HttpServletResponse.SC_OK);
		resp.getWriter ().println (html);
	}
	
}
