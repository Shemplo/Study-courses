package ru.shemplo.reactiveshop.db;

import org.springframework.data.jpa.repository.JpaRepository;

public interface UserEntityRepository extends JpaRepository <UserEntity, Long> {
    
    public UserEntity findByIdentifier (String identifier);
    
}
