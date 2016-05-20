{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module DependentTypes where

-- 自然数Natの定義
data Nat = Z | S Nat deriving (Eq, Show)


-- 長さ付きリストVectの定義
data Vect :: Nat -> * -> * where
  Nil  :: Vect 'Z a
  (:.) :: a -> Vect n a -> Vect ('S n) a

infixr 5 :.

instance Eq a => Eq (Vect n a) where
  Nil     == Nil     = True
  (x:.xs) == (y:.ys) = x == y && xs == ys

instance Show a => Show (Vect n a) where
  show xs = "[" ++ show' xs ++ "]"
    where
      show' :: Show a => Vect n a -> String
      show' Nil      = ""
      show' (y:.Nil) = show y
      show' (y:.ys)  = show y ++ ", " ++ show' ys

toList :: Vect n a -> [a]
toList Nil     = []
toList (x:.xs) = x : toList xs

head' :: Vect ('S n) a -> a
head' (x:._) = x


-- 自然数の有限集合Finの定義
data Fin :: Nat -> * where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

index :: Fin n -> Vect n a -> a
index FZ     (x:._)  = x
index (FS k) (_:.xs) = index k xs
