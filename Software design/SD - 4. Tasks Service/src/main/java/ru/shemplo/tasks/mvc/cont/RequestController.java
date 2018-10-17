package ru.shemplo.tasks.mvc.cont;

import static ru.shemplo.tasks.mvc.cont.ResponsePresets.*;

import java.util.List;

import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import ru.shemplo.tasks.db.DBAccess;
import ru.shemplo.tasks.mvc.model.ListOfTasks;

@Controller
public class RequestController {

    @Autowired
    private DBAccess db;
    
    @GetMapping (path = "/lists")
    public ModelAndView handleTaskLists () {
        ModelAndView mav = new ModelAndView ("lists");
        List <ListOfTasks> lists = db.getAllLists ();
        
        lists.forEach (l -> db.getTasksOfList (l.getID ()).forEach (l::addTask));
        mav.addObject ("listsOfTasks", lists);
        return mav;
    }
    
    @PostMapping (path = "/lists/add/{add-kind}")
    @ResponseBody
    public String handleAPIRequest (@PathVariable ("add-kind") String kind, 
                                    @RequestBody String body) {
        AddKind type = null;
        try {
            type = AddKind.valueOf (kind.toUpperCase ());
        } catch (IllegalArgumentException iae) {
            return error ("Unknown kind of object to add")
                 . toString ();
        }
        
        JSONObject input = new JSONObject (body);
        System.out.println (input);
        return type.handleRequest (db, input)
             . toString ();
    }
    
}
