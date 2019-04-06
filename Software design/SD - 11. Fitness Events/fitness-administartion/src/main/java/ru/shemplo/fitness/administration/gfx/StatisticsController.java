package ru.shemplo.fitness.administration.gfx;

import java.net.URL;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;

import java.util.Date;
import java.util.ResourceBundle;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.chart.BarChart;
import javafx.scene.chart.LineChart;
import javafx.util.Duration;

import ru.shemplo.fitness.statistics.StatisticsModule;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.annot.processor.SnowflakeInitializer;

public class StatisticsController implements Initializable, AutoCloseable {

    @Snowflake (manual = true)
    @FXML private LineChart <Date, Number> dayVisits, monthVisits, averageVisits;
    
    private StatisticsModule statisticsModule;

    private final BarChart.Series<Date, Number> daySeries = new BarChart.Series<>();
    private final BarChart.Series<Date, Number> monthSeries = new BarChart.Series<>();
    private final BarChart.Series<Date, Number> averageSeries = new BarChart.Series<>();

    private static final ZoneId timeZone = ZoneId.of("Europe/Moscow");

    @FXML private DateAxis dayVisitsX;
    @FXML private DateAxis monthVisitsX;
    @FXML private DateAxis averageVisitsX;
    
    private final Timeline reloadStstisticsCaller = new Timeline (
        new KeyFrame (Duration.millis  (0), __ -> reloadStatistics ()),
        new KeyFrame (Duration.seconds (1.5))
    );

    @Override
    public void initialize (URL location, ResourceBundle resources) {
        SnowflakeInitializer.initFields (Snowball.getContext (), this);

        dayVisitsX.setLowerBound(Date.from(Instant.now().minus(1, ChronoUnit.DAYS)));
        monthVisitsX.setLowerBound(Date.from(Instant.now().minus(30, ChronoUnit.DAYS)));
        averageVisitsX.setLowerBound(Date.from(Instant.now().minus(30, ChronoUnit.DAYS)));

        dayVisitsX.setUpperBound(Date.from(Instant.now().plus(5, ChronoUnit.MINUTES)));
        monthVisitsX.setUpperBound(Date.from(Instant.now().plus(1, ChronoUnit.HOURS)));
        averageVisitsX.setUpperBound(Date.from(Instant.now().plus(1, ChronoUnit.HOURS)));
        
        reloadStstisticsCaller.setCycleCount (Timeline.INDEFINITE);
        reloadStstisticsCaller.playFromStart ();
    }
    
    private void reloadStatistics () {
        daySeries.getData ().clear ();
        dayVisits.getData ().clear ();
        statisticsModule.getDayStatistics(LocalDateTime.now()).forEach((key, value) -> {
            daySeries.getData().add(new BarChart.Data<>(Date.from(key.atZone(timeZone).toInstant()), value));
        });
        dayVisits.getData().add(daySeries);

        monthSeries.getData ().clear ();
        monthVisits.getData ().clear ();
        statisticsModule.getMonthStatistics(LocalDateTime.now()).forEach((key, value) -> {
            monthSeries.getData().add(new BarChart.Data<>(Date.from(key.atZone(timeZone).toInstant()), value));
        });
        monthVisits.getData().add(monthSeries);

        averageSeries.getData ().clear ();
        averageVisits.getData ().clear ();
        statisticsModule.getAverageMonthStatistics(LocalDateTime.now()).forEach((key, value) -> {
            averageSeries.getData().add(new BarChart.Data<>(Date.from(key.atZone(timeZone).toInstant()), value));
        });
        averageVisits.getData().add(averageSeries);
    }

    @Override
    public void close () throws Exception {
        reloadStstisticsCaller.stop ();
    }
    
}
