{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Todo.Types
Description : Core data types for the Todo application

= Learning Points

== Algebraic Data Types (ADTs)
Haskell has two kinds of ADTs:
- Sum types (OR): Priority = High | Medium | Low
- Product types (AND): TodoItem has id AND title AND priority AND...

== deriving
GHC can automatically generate instances for common type classes.
DeriveGeneric enables automatic JSON serialization via Aeson.

== Records
Named fields with automatic getter functions.
TodoItem { todoId :: Int } creates a function todoId :: TodoItem -> Int
-}
module Todo.Types
    ( -- * Core Types
      TodoItem(..)
    , Priority(..)
    , Status(..)
      -- * Type Aliases (makes code more readable)
    , TodoId
    , TodoList
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Type alias: TodoId is just Int, but more descriptive
type TodoId = Int

-- | Sum type: Priority can be ONE of these values
-- Pattern: data TypeName = Constructor1 | Constructor2 | Constructor3
data Priority
    = High
    | Medium
    | Low
    deriving (Show, Eq, Ord, Generic)

-- Automatic JSON conversion via Generic
instance ToJSON Priority
instance FromJSON Priority

-- | Sum type for task status
data Status
    = Pending
    | InProgress
    | Completed
    deriving (Show, Eq, Generic)

instance ToJSON Status
instance FromJSON Status

-- | Product type (record): TodoItem has ALL these fields
-- Each field becomes a function: todoTitle :: TodoItem -> Text
data TodoItem = TodoItem
    { todoId        :: TodoId
    , todoTitle     :: Text
    , todoDesc      :: Maybe Text    -- Maybe = optional value (can be Nothing)
    , todoPriority  :: Priority
    , todoStatus    :: Status
    , todoCreatedAt :: UTCTime
    } deriving (Show, Eq, Generic)

instance ToJSON TodoItem
instance FromJSON TodoItem

-- | Type alias for a list of todos
type TodoList = [TodoItem]
