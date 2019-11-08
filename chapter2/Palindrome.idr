module Main

palindrome : Nat -> String -> Bool
palindrome min phrase = (length phrase) > min && (toLower phrase) == (reverse (toLower phrase))

counts : String -> (Nat, Nat)
counts phrase = (length (words phrase), length phrase)

top_ten : Ord a => List a -> List a
top_ten items = take 10 (reverse (sort items))

over_length : Nat -> List String -> Nat
over_length max xs = length (filter (\ARG => (length ARG) > max) xs)

showPalindrome: Nat -> String -> String
showPalindrome min phrase = show (palindrome min phrase)

main : IO ()
main = repl "\nEnter a string: "
            (showPalindrome 3)