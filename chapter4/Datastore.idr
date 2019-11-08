module Main
import Data.Vect

data DataStore : Type where
         MkData : (size : Nat) ->
                  (items : Vect size String) ->
                  DataStore
size: DataStore -> Nat
size (MkData size' items') = size'

items: (store: DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
    where
        addToData : Vect old String -> Vect (S old) String
        addToData [] = [newItem]
        addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

total parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                                False => Nothing
                                True => Just (Get (cast val))
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing
             
total parse : (input : String) -> Maybe Command
parse input = case span ( /= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)
             
total getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
    case (integerToFin pos (size store)) of
         Nothing => Just ("Out of range\n", store) 
         Just idx => Just ((index idx store_items), store)
             
total showSearchResults : (foundItems : List (Nat, String)) -> String
showSearchResults [] = ""
showSearchResults ((pos, item) :: xs) = ((show pos) ++ ": " ++ item  ++ "\n") ++ showSearchResults xs

total findTerm : (term : String) -> (store_items : Vect n String) -> List (Nat, String)
findTerm term [] = [] 
findTerm term (item :: remaining_items) = case isInfixOf (unpack term) (unpack item) of
                               False => findTerm term remaining_items
                               True => (1, item) :: findTerm term remaining_items

total searchString : (term : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchString term store = let store_items = items store in
  case showSearchResults (findTerm term store_items) of 
    "" => Just ("Not found\n", store)
    str => Just (str, store)

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid Command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item) 
                              Just (Get pos) => getEntry pos store
                              Just Size => Just ("Number of items: " ++ show (size store) ++ "\n", store)
                              Just (Search term) => searchString term store
                              Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput