package ru.shemplo.tasks.mvc.cont;

import static ru.shemplo.tasks.mvc.cont.ResponsePresets.*;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.NoHandlerFoundException;

import ru.shemplo.tasks.db.DBAccess;
import ru.shemplo.tasks.mvc.model.ListOfTasks;

@ControllerAdvice
@Controller
public class RequestController {

    @Autowired
    private DBAccess db;
    
    @GetMapping (path = "/")
    public ModelAndView handleIndexPage () {
        ModelAndView mav = new ModelAndView ("index");
        return mav;
    }
    
    @ResponseStatus (HttpStatus.NOT_FOUND)
    @ExceptionHandler (NoHandlerFoundException.class)
    public ModelAndView handleNotFound (HttpServletRequest req, 
            HttpServletResponse resp, Exception ex) {
        ModelAndView mav = new ModelAndView ("error");
        mav.addObject ("code", HttpStatus.NOT_FOUND.value ());
        mav.addObject ("reason", "Resource not found");
        mav.addObject ("uri", req.getRequestURL ());
        return mav;
    }
    
    @GetMapping (path = "/lists")
    public ModelAndView handleTaskLists () {
        ModelAndView mav = new ModelAndView ("lists");
        List <ListOfTasks> lists = db.getAllLists ();
        
        lists.forEach (l -> db.getTasksOfList (l.getID ()).forEach (l::addTask));
        mav.addObject ("listsOfTasks", lists);
        return mav;
    }
    
    @PostMapping (path = "/lists/{operation}/{aim}")
    @ResponseBody
    public String handleAPIRequest (@PathVariable ("operation") String operation, 
            @PathVariable ("aim") String aim, @RequestBody String body) {
        RequestOperation type = null;
        try {
            String operName = (operation + "_" + aim).toUpperCase ();
            type = RequestOperation.valueOf (operName);
        } catch (IllegalArgumentException iae) {
            return error ("Unknown operation `" + operation + "`/"
                 + "`" + aim + "`").toString ();
        }
        
        JSONObject input = new JSONObject (body);
        System.out.println ("Request " + operation + "/" + aim 
                         + " " + input);
        
        return type.handleRequest (db, input)
             . toString ();
    }
    
}
