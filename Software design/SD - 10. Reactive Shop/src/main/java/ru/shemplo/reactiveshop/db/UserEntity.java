package ru.shemplo.reactiveshop.db;

import javax.persistence.*;

import lombok.*;

@Entity
@ToString
@Getter @Setter
@NoArgsConstructor 
@AllArgsConstructor
@Table (name = "users")
public class UserEntity {
    
    @Id @GeneratedValue 
    @Access (AccessType.PROPERTY)
    private Long id;
    
    private String login;
    
    private String identifier;
    
    @ManyToOne
    private CurrencyEntity currency;
    
}
