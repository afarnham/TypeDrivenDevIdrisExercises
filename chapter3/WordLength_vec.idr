import Data.Vect

total allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words


-- allLengths [] = []
-- allLengths (word :: words) = length word :: allLengths words
