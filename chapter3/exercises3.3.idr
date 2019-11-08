import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

elemVectZip : (x : elem) -> (y : Vect len elem) -> Vect (S len) elem
elemVectZip x y = x :: y

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
  (zipWith elemVectZip x xsTrans)

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix xs ys = zipWith (zipWith (+)) xs ys -- Manuel's solution
-- addMatrix (x :: xs) (y :: ys) = (zipWith (\x,y => x + y) x y) :: (addMatrix xs ys)

dotProd : Num a => (xs : Vect len a) -> (ys : Vect len a) -> a
dotProd xs ys = foldl (+) 0 (zipWith (*) xs ys)

helper : Num a => (x : Vect m a) -> (ysTrans : Vect p (Vect m a)) -> Vect (S len) (Vect p a)
helper x [] = ?helper_rhs_1
helper x (y :: ys) = ?helper_rhs_2

-- M(3 x 2) * M(2 x 4) -> M(3 x 4)
multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] [] = []
multMatrix [] (x :: xs) = ?multMatrix_rhs_4
multMatrix (x :: xs) ys = let ysTrans = transposeMat ys in
  helper x ysTrans
