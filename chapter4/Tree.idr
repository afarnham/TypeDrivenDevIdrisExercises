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
