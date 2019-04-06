package ru.shemplo.fitness.entities;

import java.time.LocalDate;
import java.time.LocalDateTime;

import java.util.Optional;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import ru.shemplo.fitness.services.FitnessClientService;

@ToString
@Getter @Setter
@NoArgsConstructor
public class FitnessClient implements Updatable, Identifiable, Completable {
    
    private Integer id;
    
    private String firstName, 
                   secondName, 
                   lastName,
                   
                   organization,
                   position,
                   
                   country,
                   state,
                   city,
                   district,
                   
                   phone,
                   email,
                   homePage,
                   
                   remark,
                   
                   sex;
    
    private LocalDate birthday;
    
    /**
     * Only for update via {@link FitnessClientService#updateClient(FitnessClient)} purposes
     */
    private LocalDateTime lastTimeUpdated;

    @Override
    public boolean isCompleted () {
        return id != null && firstName != null && lastName != null;
    }
    
    public String getFullName () {
        String second = Optional.ofNullable (getSecondName ()).orElse ("");
        return String.format ("%s %s %s", lastName, firstName, second);
    }
    
}
