package ru.shemplo.fitness.app.services;

import android.util.Log;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import ru.shemplo.fitness.app.db.DBConnector;

public class TicketService {

    private static final String GET_TICKET_VISITS_QUERY = "SELECT es.`object_id`, es.`property_name`, es.`property_action`,\n" +
            "       es.`property_value`, es.`date` \n" +
            "FROM `events` es, (\n" +
            "    SELECT `object_id` FROM `events`\n" +
            "    WHERE `object_class` = 'ticket' AND `property_name` = 'secret'\n" +
            "        AND `property_action` = 'set' AND `property_value` = ? \n" +
            "    ORDER BY `date` DESC\n" +
            "    LIMIT 1\n" +
            ") id\n" +
            "WHERE es.`object_id` = id.`object_id` \n" +
            "ORDER BY `date` ASC";
    private static final String WITHDRAW_VISIT_QUERY = "INSERT INTO `events` (`object_class`, `object_id`, `property_name`, `property_action`, `property_value`)\n" +
            "SELECT 'ticket', `object_id`, 'visits', 'subtract', '1' FROM `events`\n" +
            "    WHERE `object_class` = 'ticket' AND `property_name` = 'secret'\n" +
            "    AND `property_action` = 'set' AND `property_value` = ?\n" +
            "ORDER BY `date` DESC\n" +
            "LIMIT 1";
    private final DBConnector connector = new DBConnector();

    public int getVisitOfTicketBySecret(String secret) throws IOException {

        Connection connection = null;
        try {
            connection = connector.openConnection();
        } catch (ClassNotFoundException e) {
            throw new IOException(e);
        }

        try {
            PreparedStatement statement = connection.prepareStatement(GET_TICKET_VISITS_QUERY);
            statement.setString(1, secret);
            ResultSet rows = statement.executeQuery();

            int visits = 0;
            while (rows.next()) {
                if (!rows.getString("property_name").equals("ticket")
                        && !rows.getString("property_name").equals("visits")) {
                    continue;
                }

                switch (rows.getString("property_action")) {
                    case "add":
                        visits += rows.getInt("property_value");
                        Log.d("DB", "ADD");
                        break;
                    case "subtract":
                        visits -= rows.getInt("property_value");
                        Log.d("DB", "SUB");
                        break;
                    case "set":
                        visits = rows.getInt("property_value");
                        Log.d("DB", "SET");
                        break;
                }
            }

            if (visits > 0) {
                PreparedStatement withdraw = connection.prepareStatement(WITHDRAW_VISIT_QUERY);
                withdraw.setString(1, secret);
                withdraw.execute();
            }

            return visits;
        } catch (SQLException sqle) {
            throw new IOException(sqle);
        }
    }

}
