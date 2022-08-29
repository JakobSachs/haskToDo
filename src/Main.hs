module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import System.Console.Readline
import System.Directory
import System.Environment
import System.Exit
import System.IO
import ToDoItem

usage = putStrLn "Usage: haskToDo [help/show/new/delete/...] ..."

-- show <list name>
-- new
-- delete

readToDoList :: String -> IO (Maybe ToDoList)
readToDoList listName = do
  json <- BS.readFile (listName <> ".json")
  return (decode json)

saveToDoList :: ToDoList -> IO ()
saveToDoList list = BS.writeFile (listTitle list <> ".json") (encode list)

showToDoList :: String -> IO ()
showToDoList listName = do
  decodedList <- readToDoList listName
  case decodedList of
    Just lst -> do
      putStrLn $ unlines . map renderItem $ items lst
    Nothing -> putStrLn "Error parsing list" >> exitFailure

getUserInput :: String -> IO String
getUserInput query = do
  mybLine <- readline (query <> "> ")
  case mybLine of
    Nothing -> putStrLn ("Please provide a " <> query) >> getUserInput query
    Just [] -> putStrLn (query <> " cant be empty!") >> getUserInput query
    Just input -> return input

-- main

main :: IO ()
main = do
  -- parse arguments
  args <- getArgs
  case args of
    ["help"] -> usage
    -- list items
    ["list"] -> do
      fileExists <- doesFileExist "main.json"
      if fileExists
        then showToDoList "main"
        else putStrLn "Nothing to-do!"

    -- add new item
    ["new"] -> do
      titel <- getUserInput "Titel"
      body <- getUserInput "Body"
      let item = ToDoItem {itemTitle = titel, status = Pending, body = body}
      -- parse list
      fileExists <- doesFileExist "main.json"
      if fileExists
        then do
          my_lst <- readToDoList "main"

          -- add new item to list and save it
          case my_lst of
            Just lst -> do
              let new_lst =
                    ToDoList
                      { listTitle = (listTitle lst),
                        items = ((items lst) ++ [item])
                      }
              saveToDoList new_lst
            Nothing -> putStrLn "failed to parse json" >> exitFailure
        else -- just save to json file
        do
          let lst = ToDoList {listTitle = "main", items = [item]}
          saveToDoList lst

    -- no input case
    _ -> putStrLn "Error parsing input (try 'help')" >> exitFailure
