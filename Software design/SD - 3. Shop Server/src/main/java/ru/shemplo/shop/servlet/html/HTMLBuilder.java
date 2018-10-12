package ru.shemplo.shop.servlet.html;

import java.util.Arrays;

import java.sql.ResultSet;
import java.sql.SQLException;

public class HTMLBuilder {

	private final StringBuilder SB = new StringBuilder ();
	
	public HTMLBuilder (String title) {
		SB.append ("<html>")
		  .append ("<head>")
		  .append ("<title>").append (title).append ("</title>")
		  .append ("<meta charset=\"UTF-8\" />")
		  .append ("<link rel=\"stylesheet\" href=\"src/main/resources/styles.css\"/>")
		  .append ("</head>")
		  .append ("<body>");
	}
	
	@Override
	public String toString () {
		return SB.toString () + "</body></html>";
	}
	
	public HTMLBuilder addHeader (String header) {
		SB.append ("<h2>").append (header).append("</h2>");
		return this;
	}
	
	public HTMLBuilder addSmallHeader (String header) {
		SB.append ("<h3>").append (header).append("</h3>");
		return this;
	}
	
	public HTMLBuilder addParagraph (String value) {
		SB.append ("<p>").append (value).append("</p>");
		return this;
	}
	
	public HTMLBuilder addTag (String value) {
		SB.append (value);
		return this;
	}
	
	public static void resultToHTML (ResultSet result, HTMLBuilder builder, String ... columns)
			throws SQLException {
		builder.addTag ("<table border=\"1\">")
			   .addTag ("<tr>");
		Arrays.asList (columns).stream ()
			  .map (c -> "<td>" + c + "</td>")
			  .forEach (builder::addTag);
		builder.addTag ("</tr>");
		
		boolean atLeastOne = false;
		while (result.next ()) {
			builder.addTag ("<tr>");
			Arrays.asList (columns).forEach (c -> {
				try {
					builder.addTag ("<td>").addTag (result.getString (c))
						   .addTag ("</td>");
				} catch (SQLException sqle) {
					throw new IllegalStateException (sqle);
				}
			});
			builder.addTag ("</tr>");
			
			atLeastOne = true;
		}
		
		if (!atLeastOne) {
			builder.addTag ("<td colspan=\"" + columns.length + "\">")
				   .addTag ("no such objects in database").addTag ("</td>");
		}
		
		builder.addHeader ("</table>");
	}
	
}
