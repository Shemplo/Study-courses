package ru.shemplo.reactiveshop.db;

import javax.persistence.*;

import lombok.*;

@Entity
@ToString
@Getter @Setter
@NoArgsConstructor 
@AllArgsConstructor
@Table (name = "currencies")
@EqualsAndHashCode (exclude = {"id"})
public class CurrencyEntity {
    
    @Id @GeneratedValue 
    @Access (AccessType.PROPERTY)
    private Long id;
    
    private String codeISO;
    
}
