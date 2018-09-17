module Natural where

data Nat = Z | S Nat

instance Eq Nat where
  (==) Z Z = True
  (==) (S a) (S b) = a == b
  (==) _ _ = False

instance Num Nat where
  (+) x Z = x
  (+) Z y = y
  (+) x (S y) = S (x + y)

  (-) x Z = x
  (-) Z _ = Z
  (-) (S x) (S y) = x - y

  (*) _ Z = Z
  (*) Z _ = Z
  (*) x (S y) = x + (x * y)

  abs = id
  
  signum Z = 0
  signum _ = 1

  fromInteger 0 = Z
  fromInteger x = S (fromInteger (x - 1))

instance Ord Nat where
  (<=) (S _) Z = False
  (<=) (S x) (S y) = x <= y
  (<=) _ _ = True

instance Enum Nat where
  pred a = a - 1

  fromEnum Z     = 0
  fromEnum (S a) = 1 + fromEnum a

  toEnum a
       | a == 0 = Z
       | a > 0  = S (toEnum (a - 1))
       | otherwise = error "Not Nat"