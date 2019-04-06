package ru.shemplo.fitness.statistics;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import java.util.concurrent.ConcurrentHashMap;

import ru.shemplo.fitness.entities.FitnessEvent;
import ru.shemplo.fitness.entities.SeasonTicket;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.stuctures.Pair;

@Snowflake
public class StatisticsModule {
    
    private final Map <Integer, Map <LocalDateTime, Integer>> 
        statisticsPerClient = new HashMap <> ();
    
    private final Map <Integer, Integer> ticket2client = new HashMap <> ();
    
    public void onNewTicketEvents (List <FitnessEvent> events) {
        String objectClass = SeasonTicket.class.getSimpleName ().toLowerCase ();
        events.stream ().filter (e -> e.getObjectClass ().equals (objectClass))
              .forEach (event -> {
                  String property = event.getPropertyName   (),
                         action   = event.getPropertyAction ();
                  if (property.equals ("client")) {
                      if (action.equals ("set")) {
                          int clientID = Integer.parseInt (event.getPropertyValue ());
                          ticket2client.put (event.getObjectId (), clientID);
                      }
                  } else if (property.equals ("visits")) {
                      if (action.equals ("subtract")) {
                          final int amount = Integer.parseInt (event.getPropertyValue ());
                          final LocalDateTime time = event.getDate ().toLocalDateTime ();
                          Integer clientID = ticket2client.get (event.getObjectId ());
                          if (amount == 1 && clientID != null) {
                              addVisitToClient (clientID, time);
                          } else {
                              System.err.println("Unknown client: " + event);
                          }
                      }
                  }
              });
    }
    
    private synchronized void addVisitToClient (int clientID, LocalDateTime time) {
        statisticsPerClient.putIfAbsent (clientID, new ConcurrentHashMap<> ());
        statisticsPerClient.get (clientID).compute (time, (__, v) -> v == null ? 1 : v + 1);
    }
    
    public Map <LocalDateTime, Integer> getDayStatistics (LocalDateTime time) {
        LocalDateTime day = time.truncatedTo (ChronoUnit.DAYS);
        
        return statisticsPerClient.values ().stream ()
             . map     (Map::entrySet).flatMap (Set::stream)
             . map     (Pair::fromMapEntry)
             . filter  (p -> day.isBefore (p.F) || day.isEqual (p.F))
             . map     (p -> p.applyF (t -> t.truncatedTo (ChronoUnit.HOURS)))
             . collect (Collectors.toMap (Pair::getF, Pair::getS, (a, b) -> a + b));
    }
    
    public Map <LocalDateTime, Integer> getMonthStatistics (LocalDateTime time) {
        LocalDateTime day = time.withDayOfMonth (1).withHour (0)
                                .withMinute (0).withSecond (0);
        
        return statisticsPerClient.values ().stream ()
             . map     (Map::entrySet).flatMap (Set::stream)
             . map     (Pair::fromMapEntry)
             . filter  (p -> day.isBefore (p.F) || day.isEqual (p.F))
             . map     (p -> p.applyF (t -> t.truncatedTo (ChronoUnit.DAYS)))
             . collect (Collectors.toMap (Pair::getF, Pair::getS, (a, b) -> a + b));
    }
    
    public Map <LocalDateTime, Double> getAverageMonthStatistics (LocalDateTime time) {
        Double clients = (double) ticket2client.size ();
        return getMonthStatistics (time).entrySet ().stream ()
             . map     (Pair::fromMapEntry)
             . map     (p -> p.applyS (Number::doubleValue))
             . map     (p -> p.applyS (v -> v / clients))
             . collect (Collectors.toMap (Pair::getF, Pair::getS));
    }
    
}
