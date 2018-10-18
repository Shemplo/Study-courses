package ru.shemplo.tasks.db;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import ru.shemplo.snowball.utils.db.DBType;
import ru.shemplo.tasks.mvc.model.ListOfTasks;
import ru.shemplo.tasks.mvc.model.Task;

public class DBAccess {

    private DBConnection db;
    
    private final String 
        SQL_SELECT_ALL_LISTS     = "SELECT * FROM `lists` ORDER BY `id`",
        SQL_SELECT_TASKS_BY_LIST = "SELECT * FROM `tasks` WHERE `list`=':list' ORDER BY `id`",
        SQL_SELECT_TASK          = "SELECT * FROM `tasks` WHERE `id`=':id' LIMIT 1",
        SQL_INSERT_LIST          = "INSERT INTO `lists` (`title`) VALUES (':title')",
        SQL_DELETE_LIST          = "DELETE FROM `lists` WHERE `id` = ':id' LIMIT 1",
        SQL_INSERT_TASK          = "INSERT INTO `tasks` (`list`, `desc`, `status`) "
                                 + "VALUES (':list', ':desc', '0')",
        SQL_INSERT_EXPIRE_TASK   = "INSERT INTO `tasks` (`list`, `desc`, `expire`, `status`) "
                                 + "VALUE (':list', ':desc', ':expire', '0')",
        SQL_DELETE_TASK          = "DELETE FROM `tasks` WHERE `id` = ':id' LIMIT 1",
        SQL_CHANGE_TASK_STATUS   = "UPDATE `tasks` SET `status` = ':status' "
                                 + "WHERE `id` = ':id' LIMIT 1";
    
    public static final DateFormat 
        SQL_FORMAT = new SimpleDateFormat ("yyyy-MM-dd HH:mm");
    
    public DBAccess () {
        this.db = DBConnection.getInstanceOf (DBType.MySQL);
    }
    
    public List <ListOfTasks> getAllLists () {
        try {
            ResultSet result = db.execute (SQL_SELECT_ALL_LISTS);
            List <ListOfTasks> lists = new ArrayList <> ();
            
            while (result.next ()) {
                lists.add (ListOfTasks.valueFrom (result));
            }
            
            return lists;
        } catch (SQLException sqle) {}
        
        return new ArrayList <> ();
    }
    
    public List <Task> getTasksOfList (long listID) {
        try {
            String query = SQL_SELECT_TASKS_BY_LIST
                         . replace (":list", "" + listID);
            List <Task> tasks = new ArrayList <> ();
            ResultSet result = db.execute (query);
            
            while (result.next ()) {
                tasks.add (Task.valueFrom (result));
            }
            
            return tasks;
        } catch (SQLException sqle) {}
        
        return new ArrayList <> ();
    }
    
    public Task getTask (long taskID) {
        try {
            String query = SQL_SELECT_TASK.replace (":id", "" + taskID);
            ResultSet result = db.execute (query);
            
            if (result.next ()) { return Task.valueFrom (result); }
        } catch (SQLException sqle) {}
        
        return null;
    }
    
    public void addList (String title) throws SQLException {
        db.update (SQL_INSERT_LIST.replace (":title", title));
    }
    
    public void deleteList (long listID) throws SQLException {
        db.update (SQL_DELETE_LIST.replace (":id", "" + listID));
    }
    
    public void addTask (long listID, String description, Date expire) throws SQLException {
        String query = expire == null ? SQL_INSERT_TASK : SQL_INSERT_EXPIRE_TASK;
        query = query.replace (":list", "" + listID).replace (":desc", description);
        
        if (expire != null) {
            query = query.replace (":expire", SQL_FORMAT.format (expire));
        }
        
        db.update (query);
    }
    
    public void deleteTask (long taskID) throws SQLException {
        db.update (SQL_DELETE_TASK.replace (":id", "" + taskID));
    }
    
    public void setTaskStatus (long taskID, int status) throws SQLException {
        db.update (SQL_CHANGE_TASK_STATUS.replace (":id", "" + taskID)
                                         .replace (":status", "" + status));
    }
    
}
