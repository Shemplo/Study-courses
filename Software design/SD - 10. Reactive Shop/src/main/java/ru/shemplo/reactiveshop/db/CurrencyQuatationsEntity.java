package ru.shemplo.reactiveshop.db;

import java.util.Date;

import javax.persistence.*;

import lombok.*;

@Entity
@ToString
@Getter @Setter
@NoArgsConstructor 
@AllArgsConstructor
@Table (name = "quotations")
public class CurrencyQuatationsEntity {
    
    @Id @GeneratedValue 
    @Access (AccessType.PROPERTY)
    private Long id;
    
    @ManyToOne
    private CurrencyEntity currency;
    
    private double price;
    
    private Date responsibleTime;
    
}
