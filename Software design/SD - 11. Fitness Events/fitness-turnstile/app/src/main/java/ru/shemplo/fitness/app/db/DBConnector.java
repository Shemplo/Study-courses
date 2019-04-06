package ru.shemplo.fitness.app.db;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

import static ru.shemplo.fitness.app.db.DBConfiguration.LOGIN;
import static ru.shemplo.fitness.app.db.DBConfiguration.PASSWORD;
import static ru.shemplo.fitness.app.db.DBConfiguration.URL;

public class DBConnector {

    public Connection openConnection() throws IOException, ClassNotFoundException {
        Class.forName("com.mysql.jdbc.Driver");
        try {
            return DriverManager.getConnection (URL, LOGIN, PASSWORD);
        } catch (SQLException sqle) { throw new IOException (sqle); }
    }

}
