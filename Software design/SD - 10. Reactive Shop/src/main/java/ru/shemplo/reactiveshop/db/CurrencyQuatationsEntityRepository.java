package ru.shemplo.reactiveshop.db;

import org.springframework.data.jpa.repository.JpaRepository;

public interface CurrencyQuatationsEntityRepository extends JpaRepository <CurrencyQuatationsEntity, Long> {
    
    public CurrencyQuatationsEntity findByCurrencyOrderByResponsibleTime (CurrencyEntity currency);
    
}
