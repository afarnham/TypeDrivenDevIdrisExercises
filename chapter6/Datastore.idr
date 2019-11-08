module Main
import Data.Vect

infixr 5 .+.
data Schema = SString
              | SInt
              | SChar
              | (.+.) Schema Schema
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
    constructor MkData
    schema : Schema
    size : Nat
    items : Vect size (SchemaType schema)



total addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newItem = MkData schema _ (addToData store)
    where
        addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
        addToData [] = [newItem]
        addToData (item :: items) = item :: addToData items

data Command : Schema -> Type where
    SetSchema : (newschema : Schema) -> Command schema
    Add : SchemaType schema -> Command schema
    Get : Maybe Integer -> Command schema
    Quit : Command schema


total parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
  = case xs of 
         [] => Just SString
         _ => case parseSchema xs of
            Nothing => Nothing
            Just xs_schema => Just (SString .+. xs_schema)
parseSchema ("Int" :: xs) 
  = case xs of 
         [] => Just SInt
         _ => case parseSchema xs of
            Nothing => Nothing
            Just xs_schema => Just (SInt .+. xs_schema)
parseSchema ("Char" :: xs)
  = case xs of 
         [] => Just SChar
         _ => case parseSchema xs of 
            Nothing => Nothing
            Just xs_schema => Just (SChar .+. xs_schema)
parseSchema _ = Nothing

total parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input = case unpack input of
                                (x :: xs) => Just (x, ltrim(pack xs))
                                _ => Nothing

parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing => Nothing
                                               Just (l_val, input') => 
                                                    case parsePrefix schemar input' of
                                                        Nothing => Nothing
                                                        Just (r_val, input'') => Just ((l_val, r_val), input'')
                                                    
total parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Nothing => Nothing
                                  Just (res, "") => Just res
                                  Just _ => Nothing

total parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "schema" rest = case parseSchema (words rest) of
                                         Just (schema) => Just (SetSchema schema)
                                         Nothing => Nothing

parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      Just (restok) => Just (Add restok)
parseCommand schema "get" val = let chars = unpack val in 
  case chars of
    [] => Just (Get Nothing)
    _ => case all isDigit chars of
      True => Just (Get (Just (cast val)))
      False => Nothing
-- parseCommand "size" "" = Just Size
-- parseCommand "search" str = Just (Search str)
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span ( /= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)


total display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ "," ++ display itemr

total getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
         case integerToFin pos (size store) of
              Nothing => Just ("Out of range\n", store) 
              Just id => Just ( (show pos) ++ ":" ++ display (index id (items store)) ++ "\n",
                               store)

-- getAllEntries : (store : DataStore) -> Maybe (String, DataStore)
-- getAllEntries store = getEntries (size store) store
--   where
getEntries : Nat -> (store : DataStore) -> Maybe (String, DataStore)
getEntries Z store = pure ("", store)
getEntries (S k) store = case getEntry (toIntegerNat k) store of 
                              Just (str, store') => case getEntries k store' of 
                                                         Just (str', store'') => Just(str' ++ str, store'')
                                                         Nothing => Nothing
                              Nothing => Nothing
        


{- 
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
  -}

total setSchema : (store : DataStore) -> Schema ->  Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S k => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                              Nothing => Just ("Invalid Command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get pos) => case pos of 
                                Just pos' => getEntry pos' store
                                Nothing => getEntries (size store) store
                              Just (SetSchema schema') =>
                                case setSchema store schema' of
                                    Nothing => Just ("Can't update schema when store has entries\n", store)
                                    Just store' => Just ("OK\n", store')
                              --Just Size => Just ("Number of items: " ++ show (size store) ++ "\n", store)
                              --Just (Search term) => searchString term store
                              Just Quit => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput