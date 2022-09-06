module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import System.Console.Readline
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix
import ToDoItem

usage = putStrLn "Usage: haskToDo [help/list/new/done/prune/...]"

--
-- utility functions
--

getToDoList :: String -> IO ToDoList
getToDoList listName = do
  usrHome <- getUserHome
  let fileName = usrHome ++ "/.haskToDo/" ++ listName ++ ".json"
  doesFileExist <- fileExist fileName
  if doesFileExist
    then do
      json <- BS.readFile fileName
      let maybeLst = decode json
      case maybeLst of
        Just lst -> return lst
        Nothing -> putStrLn "Failure to parse json" >> exitFailure >> return (ToDoList {listTitle = listName, items = []})
    else return (ToDoList {listTitle = listName, items = []})

saveToDoList :: ToDoList -> IO ()
saveToDoList list = do
  usrHome <- getUserHome
  let fileName = usrHome ++ "/.haskToDo/" ++ (listTitle list) ++ ".json"
  BS.writeFile fileName (encode list)

showToDoList :: String -> IO ()
showToDoList listName = do
  lst <- getToDoList listName
  putStrLn $ unlines . map renderItem $ items lst

getUserInput :: String -> IO String
getUserInput query = do
  mybLine <- readline (query ++ "> ")
  case mybLine of
    Nothing -> putStrLn ("Please provide a " ++ query) >> getUserInput query
    Just [] -> putStrLn (query ++ " cant be empty!") >> getUserInput query
    Just input -> return input

getConfirmation :: IO Bool
getConfirmation = do
  mybResp <- readline "Please confirm (y/N)> "
  case mybResp of
    Just "y" -> return True
    Nothing -> return False
    Just _ -> return False

getUserHome :: IO String
getUserHome = homeDirectory <$> (getUserEntryForID =<< getRealUserID)

--
-- main
--

main :: IO ()
main = do
  usr_home <- getUserHome
  createDirectoryIfMissing False (usr_home ++ "/.haskToDo")

  -- parse arguments
  args <- getArgs
  case args of
    ["help"] -> usage
    ["list"] -> showToDoList "main"
    --
    -- add new item
    --
    ["new"] -> do
      titel <- getUserInput "Titel"
      body <- getUserInput "Body"
      let item = ToDoItem {itemTitle = titel, status = Pending, body = body}
      -- parse list
      lst <- getToDoList "main"
      -- add new item to list and save it
      let new_lst =
            ToDoList
              { listTitle = (listTitle lst),
                items = ((items lst) ++ [item])
              }
      saveToDoList new_lst

    --
    -- change items status
    --
    ["done"] -> do
      titel <- getUserInput "Titel"
      fileExists <- doesFileExist "main.json"
      if fileExists
        then do
          lst <- getToDoList "main"

          -- add new item to list and save it
          let orig_items = items lst
          let old_item = head $ filter (\x -> itemTitle x == titel) orig_items
          let new_item = old_item {status = Done}
          let new_lst = lst {items = ((filter (\x -> itemTitle x /= titel) orig_items) ++ [new_item])}
          saveToDoList new_lst
        else do
          -- just save to json file
          putStrLn "No current tasks"
    --
    -- remove items with Done status
    --
    ["prune"] -> do
      -- read in list, if file exists
      lst <- getToDoList "main"
      let amountOfDoneItems = length (filter (\x -> status x == Done) $ items lst)
      case amountOfDoneItems of
        0 -> putStrLn "No items to cleanup!"
        _ -> do
          putStrLn $ "Are you sure you want to delete " ++ show amountOfDoneItems ++ " item(s)?"
          confirm <- getConfirmation
          if confirm
            then do
              -- filer out done items
              let new_lst = lst {items = (filter (\x -> status x /= Done) $ items lst)}
              saveToDoList new_lst
              putStrLn "Cleaned items"
            else do
              putStrLn "Stopping"

    -- no input case
    _ -> putStrLn "Error parsing input (try 'help')" >> exitFailure
