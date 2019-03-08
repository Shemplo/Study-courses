package ru.shemplo.reactiveshop.db;

import org.springframework.data.jpa.repository.JpaRepository;

public interface CurrencyEntityRepository extends JpaRepository <CurrencyEntity, Long> {
    
    public CurrencyEntity findByCodeISO (String codeISO);
    
}
