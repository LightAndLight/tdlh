module Main where

import Control.Monad
import Control.Monad.State
import Data.Char 
import System.Exit

data Input = Input String [String]

data Command
  = Quit
  | Help
  | List
  | Add String
  | Remove Int
  | Save String
  | Load String
  deriving (Show, Eq)

data InputError
  = MissingArgs
  | NotAnInt String
  | InvalidCommand String

showInputError :: InputError -> String
showInputError err =
  case err of
    MissingArgs -> "Missing argument"
    NotAnInt val -> quoted val ++ " is not an integer"
    InvalidCommand cmd -> quoted cmd ++ " is not a valid command"
  where
    quoted str = "'" ++ str ++ "'"

lineToInput :: String -> Input
lineToInput line =
  case separate ':' line of
    [] -> Input "" []
    cmd:args -> Input cmd args
  where
    separate :: Eq a => a -> [a] -> [[a]]
    separate sep [] = []
    separate sep input =
      let (chunk, rest) = break (==sep) input
          toPrepend =
            case chunk of
              [] -> []
              _ -> [chunk]
      in case rest of
        [] -> toPrepend
        (_:rest) -> toPrepend ++ separate sep rest

inputToCommand :: Input -> Either InputError Command
inputToCommand (Input cmd args) =
  case fmap toLower cmd of
    "quit" -> Right Quit
    "help" -> Right Help
    "list" -> Right List
    "add" -> withNArgs 1 args $ \[message] -> Right $ Add message
    "remove" -> withNArgs 1 args $ \[ix] ->
      if all isDigit ix
        then Right . Remove $ read ix
        else Left $ NotAnInt ix
    "save" -> withNArgs 1 args $ \[path] -> Right $ Save path
    "load" -> withNArgs 1 args $ \[path] -> Right $ Load path
    cmd -> Left $ InvalidCommand cmd
  where
    withNArgs :: Int -> [a] -> ([a] -> Either InputError r) -> Either InputError r
    withNArgs 0 xs f = f xs
    withNArgs n xs f =
      case take n xs of
        [] -> Left MissingArgs
        xs' -> f xs'

helpMessage :: String
helpMessage = unlines
  [ "Commands:"
  , ""
  , "quit - Exit the program"
  , "help - Display this message"
  , "list - Display the list"
  , "add:<string> - Adds a message to the list"
  , "remove:<int> - Removes the element at the specified position"
  , "save:<filepath> - Save the list to the specified file"
  , "load:<filepath> - Load a list from the specified file"
  ]

showTodos :: [String] -> String
showTodos [] = "List is empty"
showTodos xs = unlines . fmap (\(n,x) -> show n ++ ": " ++ x) $ zip [0..] xs

runCommand :: Command -> StateT [String] IO ()
runCommand Quit = liftIO exitSuccess
runCommand Help = liftIO $ putStrLn helpMessage
runCommand List = do
  list <- get
  liftIO . putStrLn $ showTodos list 
runCommand (Add message) = modify (++[message])
runCommand (Remove ix) = modify (removeAt ix)
  where
    removeAt _ [] = []
    removeAt 0 (_:xs) = xs
    removeAt n (x:xs) = x : removeAt (n-1) xs
runCommand (Save path) = do
  list <- get
  liftIO . writeFile path $ unlines list
runCommand (Load path) = do
  list <- fmap lines . liftIO $ readFile path
  put list

program = do
  input <- liftIO getLine
  unless (null input) $ do
    liftIO $ putStrLn ""
    case inputToCommand $ lineToInput input of
      Right cmd -> runCommand cmd
      Left err ->
        liftIO $ do
          putStrLn $ showInputError err
          putStrLn "Type 'help' to see available commands\n"
  program

main = runStateT program []
