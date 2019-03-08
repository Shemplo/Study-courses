package ru.shemplo.reactiveshop.db;

import javax.persistence.*;

import lombok.*;

@Entity
@ToString
@Getter @Setter
@NoArgsConstructor 
@AllArgsConstructor
@Table (name = "items")
public class ItemEntity {
    
    @Id @GeneratedValue 
    @Access (AccessType.PROPERTY)
    private Long id;
    
    private String name;
    
    private double price;
    
}
