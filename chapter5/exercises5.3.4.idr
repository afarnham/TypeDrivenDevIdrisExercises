import Data.Vect

-- Exercise 1
readToBlank : IO (List String)
readToBlank = do line <- getLine
                 if (line == "")
                    then pure []
                    else do lines <- readToBlank
                            pure (line :: lines)

-- Exercise 2
list2String : (List String) -> String
list2String [] = ""
list2String (x :: xs) = x ++ "\n" ++ (list2String xs)

readAndSave : IO ()
readAndSave = do xs <- readToBlank
                 filename <- getLine
                 writeFile filename (list2String xs)
                 pure ()


-- Exercise 3
readVect : (fHandle : File) -> IO (len ** Vect len String)
readVect fHandle = do Right line <- fGetLine fHandle
                        | Left err =>  do putStrLn (show err)
                                          pure (_ ** [])
                      if (line == "")
                          then pure (_ ** [])
                          else do (_ ** lines) <- readVect fHandle
                                  pure (_ ** line :: lines)


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right h <- openFile filename Read 
                             | Left err => do putStrLn (show err)
                                              pure (_ ** [])
                           (_ ** vect) <- readVect h
                           pure (_ ** vect)
