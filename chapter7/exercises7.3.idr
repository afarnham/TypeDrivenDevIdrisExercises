
data Vect : Nat -> Type -> Type where
    Nil : Vect z elem
    (::) : elem -> Vect k elem -> Vect (S k) elem


-- Why is the following not total?
(Eq elem) => Eq (Vect k elem) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys
  (/=) [] [] = False
  (/=) (x :: xs) (y :: ys) = x /= y || xs /= ys


-- Foldable is generic so we only need (Vect k) the other parameter for Vect is provided as part of Foldable def (a generic value)
Foldable (Vect k) where
    foldr func acc [] = acc
    foldr func acc (x :: xs) = func x (foldr func acc xs)
    foldl func acc [] = acc
    foldl func acc (x :: xs) = func (foldl func acc xs) x