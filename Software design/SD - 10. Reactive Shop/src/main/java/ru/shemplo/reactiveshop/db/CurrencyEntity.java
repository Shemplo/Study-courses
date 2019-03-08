package ru.shemplo.reactiveshop.db;

import javax.persistence.*;

import lombok.*;

@Entity
@ToString
@Getter @Setter
@NoArgsConstructor 
@AllArgsConstructor
@Table (name = "currencies")
public class CurrencyEntity {
    
    @Id @GeneratedValue 
    @Access (AccessType.PROPERTY)
    private Long id;
    
    private String codeISO;
    
}
