longestLength : String -> String -> Nat
longestLength x y = let len_x = length x in
                        let len_y = length y in
                            case compare len_x len_y of
                                 LT => len_y
                                 EQ => len_x
                                 GT => len_x

printLonger : IO ()
printLonger = do putStr "First string: "
                 first <- getLine
                 putStr "Second string: "
                 second <- getLine
                 putStrLn (show (longestLength first second))

printLonger2 : IO ()
printLonger2 = putStr "First string: " >>= \_ =>
               getLine >>= \first =>
                           putStr "Second string: " >>= \_ =>
                           getLine >>= \second =>
                                       putStrLn (show (longestLength first second))
