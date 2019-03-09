package ru.shemplo.reactiveshop.db;

import javax.persistence.*;

import lombok.*;

@Entity
@Builder
@ToString
@Getter @Setter
@NoArgsConstructor 
@AllArgsConstructor
@Table (name = "users", uniqueConstraints = {
            @UniqueConstraint (columnNames = {"identifier"})
        })
@EqualsAndHashCode (exclude = {"id", "withIcon", "withDescription", "sorting", 
                               "shape", "color", "currency"})
public class UserEntity {
    
    @Id @GeneratedValue 
    @Access (AccessType.PROPERTY)
    private Long id;
    
    private String login;
    
    private String identifier;
    
    @ManyToOne
    private CurrencyEntity currency;
    
    private boolean withIcon, withDescription;
    
    private String sorting, shape, color;
    
}
