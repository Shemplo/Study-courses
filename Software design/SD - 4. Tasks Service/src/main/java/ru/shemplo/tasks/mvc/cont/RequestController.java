package ru.shemplo.tasks.mvc.cont;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
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
        mav.addObject ("listsOfTasks", lists);
        return mav;
    }
    
}
