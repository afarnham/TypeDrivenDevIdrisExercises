module Main
import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing




guess : (target : Nat) -> (guesses: Nat) -> IO()
guess target guesses = do putStrLn ("Number of guesses: " ++ (show guesses))
                          putStrLn "Enter a guess: "
                          Just num <- readNumber 
                            | Nothing => do putStr "Invalid input. Quiting."
                                            pure ()
                          case compare num target of
                            LT => do putStrLn "Too low, try again."
                                     guess target (succ guesses)                
                            EQ => do putStrLn "Correct! Quiting."
                                     pure()
                            GT => do putStrLn "Too high, try again."
                                     guess target (succ guesses)


main : IO()
main = do 
    currTime <- time
    guess (cast (mod currTime 100)) 0
    --guess (modNatNZ (fromIntegerNat currTime) 100 SIsNotZ)

myRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myRepl prompt onInput = do putStr prompt
                           input <- getLine
                           myRepl (onInput input) onInput


myReplWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt onInput = do putStr prompt
                                     input <- getLine
                                     case (onInput state input) of
                                        Just (nextPrompt, newState) => do myReplWith newState nextPrompt onInput
                                        Nothing => do pure()
