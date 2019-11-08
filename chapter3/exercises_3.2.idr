import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]
-- below was my answer... looked up the authors answer and using that above
-- my_reverse (x :: xs) = last (x :: xs) :: my_reverse (take (my_length xs) (x :: xs))


my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs


my_vect_map : (a -> b) -> Vect len a -> Vect len b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs
