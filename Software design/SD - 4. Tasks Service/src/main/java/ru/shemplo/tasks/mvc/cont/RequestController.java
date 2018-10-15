package ru.shemplo.tasks.mvc.cont;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class RequestController {

    @GetMapping (path = "/lists")
    public ModelAndView handleTaskLists () {
        ModelAndView mav = new ModelAndView ("lists");
        return mav;
    }
    
}
