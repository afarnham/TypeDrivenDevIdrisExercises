data Shape = Triangle Double Double
            | Rectangle Double Double
            | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
            | Combine Picture Picture
            | Rotate Double Picture
            | Translate Double Double Picture


rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
             (Combine (Translate 35 5 circle) 
             (Translate 15 25 triangle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic



data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
  
%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left value right) 
    = case compare x value of
        LT => Node (insert x left) value right 
        EQ => orig
        GT => Node left value (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) =  (treeToList left) ++ [x] ++ (treeToList right)

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)


maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe (Just x) Nothing = Just x
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) (Just y) = case compare x y of
                                  LT => Just y
                                  EQ => Just x
                                  GT => Just x



onlyTriangleSize : (shape : Shape) -> Maybe Double
onlyTriangleSize shape = case shape of
                              (Triangle x y) => Just (area shape)
                              (Rectangle x y) => Nothing
                              (Circle x) => Nothing


biggestTriangle: Picture -> Maybe Double
biggestTriangle pic = case pic of
                          (Primitive shape) => onlyTriangleSize shape
                          (Combine pic1 pic2) => maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
                          (Rotate x pic1) => biggestTriangle pic1
                          (Translate x y pic1) => biggestTriangle pic1

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))