package ru.shemplo.fitness.db;

import java.io.IOException;
import java.sql.SQLException;

public interface DBUpdateManager {
    
    public void update (String request) throws IOException, SQLException;
    
}
