package ru.shemplo.reactiveshop.db;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

public interface CurrencyQuatationsEntityRepository extends JpaRepository <CurrencyQuatationsEntity, Long> {
    
    public List <CurrencyQuatationsEntity> findByCurrencyOrderByResponsibleTimeDesc (CurrencyEntity currency);
    
    //@Query (value = "SELECT cq FROM CurrencyQuatationsEntity cq WHERE cq.currency = ?1 ORDER BY cq.responsibleTime DESC")
    default public CurrencyQuatationsEntity findLastByCurrency (CurrencyEntity currency) {
        return findByCurrencyOrderByResponsibleTimeDesc (currency).get (0);
    }
    
}
