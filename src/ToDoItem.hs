{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module ToDoItem where

import Data.Aeson
import GHC.Generics

data Status = Pending | Done deriving (Eq, Generic, Show)

instance ToJSON Status

instance FromJSON Status

data ToDoItem = ToDoItem
  { itemTitle :: String,
    status :: Status,
    body :: String
  }
  deriving (Generic, Show)

instance ToJSON ToDoItem

instance FromJSON ToDoItem

data ToDoList = ToDoList
  { listTitle :: String,
    items :: [ToDoItem]
  }
  deriving (Generic, Show)

instance ToJSON ToDoList

instance FromJSON ToDoList

renderItem :: ToDoItem -> String
renderItem ToDoItem {itemTitle, status, body} =
  itemTitle <> "\t\t[" <> show status <> "]\n"
    <> "----------------------"
    <> "\n"
    <> body
