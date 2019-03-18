package ru.shemplo.fitness.db;

import java.io.IOException;
import java.sql.SQLException;

import java.util.List;

public interface DBRetriveManager {
    
    public <T> List <T> retrieve (String request, Class <T> type) throws IOException, SQLException;
    
}
