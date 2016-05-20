module DependentTypes

%default total

-- 自然数Natの定義
data Nat' : Type where
  Z' : Nat'
  S' : Nat' -> Nat'

Eq Nat' where
  Z'     == Z'     = True
  (S' n) == (S' m) = n == m
  _      == _      = False

Show Nat' where
  show Z'      = "Z'"
  show (S' Z') = "S' Z'"
  show (S' n)  = "S' (" ++ show n ++ ")"


-- 長さ付きリストVectの定義
data Vect' : Nat' -> Type -> Type where
  Nil : Vect' Z' a
  (::) : a -> Vect' n a -> Vect' (S' n) a

Eq a => Eq (Vect' n a) where
  Nil     == Nil     = True
  (x::xs) == (y::ys) = x == y && xs == ys

Show a => Show (Vect' n a) where
  show xs = "[" ++ show' xs ++ "]"
    where
      show' : Vect' n a -> String
      show' Nil     = ""
      show' [x]     = show x
      show' (y::ys) = show y ++ ", " ++ show' ys

toList' : Vect' n a -> List a
toList' Nil     = []
toList' (x::xs) = x :: toList' xs

head'' : Vect' (S' n) a -> a
head'' (x::_) = x

-- dependent pair
fromList : List a -> (n ** Vect' n a)
fromList [] = (Z' ** Nil)
fromList (x::xs) with (fromList xs)
  | (_ ** xs') = (_ ** (x :: xs'))

filter' : (a -> Bool) -> Vect' n a -> (m ** Vect' m a)
filter' p Nil = (Z' ** Nil)
filter' p (x::xs) with (filter' p xs)
  | (_ ** xs') = if p x then (_ ** (x :: xs')) else (_ ** xs')


-- 自然数の有限数合Finの定義
data Fin' : Nat' -> Type where
  FZ' : Fin' (S' n)
  FS' : Fin' n -> Fin' (S' n)

index'' : Fin' n -> Vect' n a -> a
index'' FZ' (x::_)      = x
index'' (FS' k) (_::xs) = index'' k xs
