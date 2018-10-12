package ru.shemplo.shop.servlet;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class ResourceServlet extends HttpServlet {

    /**
     * 
     */
    private static final long serialVersionUID = 4238246111501489619L;
    
    @Override
    protected void doGet (HttpServletRequest req, HttpServletResponse resp) 
            throws ServletException, IOException {
        Path resource = Paths.get (req.getRequestURI ().substring (1));
        if (!Files.exists (resource)) {
            System.out.println ("Not found: " + resource);
            resp.setStatus (HttpServletResponse.SC_NOT_FOUND);
        }
        
        String fileName = resource.getFileName ().toString ();
        String contentType = getServletContext ().getMimeType (fileName);
        
        resp.setHeader ("Content-Length", "" + Files.size (resource));
        resp.setStatus (HttpServletResponse.SC_OK);
        resp.setContentType (contentType);
        
        Files.copy (resource, resp.getOutputStream ());
    }

}
