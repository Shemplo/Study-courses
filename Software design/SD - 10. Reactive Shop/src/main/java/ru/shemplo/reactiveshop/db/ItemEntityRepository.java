package ru.shemplo.reactiveshop.db;

import org.springframework.data.jpa.repository.JpaRepository;

public interface ItemEntityRepository extends JpaRepository <ItemEntity, Long> {
    
}
