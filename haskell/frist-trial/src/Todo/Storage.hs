{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Todo.Storage
Description : JSON file persistence for todos

= Learning Points

== IO Monad
All side effects (file I/O, printing) happen in the IO monad.
Functions returning IO cannot be called from pure functions.

== Either for Error Handling
Either Left Right: Left = error, Right = success
More informative than Maybe (which only has Nothing for failure)

== do-notation
Syntactic sugar for chaining monadic operations.
Each <- extracts the value from the monad.
-}
module Todo.Storage
    ( loadTodos
    , saveTodos
    , defaultFilePath
    ) where

import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)

import Todo.Types (TodoList)

-- | Default file path for todo storage
defaultFilePath :: FilePath
defaultFilePath = "todos.json"

{- |
Load todos from a JSON file.

= Monad Operations Explained

@
do
    exists <- doesFileExist path  -- IO Bool -> Bool (extract from IO)
    if exists
        then ...
        else return []            -- Put [] into IO context
@

The 'return' here is NOT like other languages!
return :: a -> m a  (wraps a value in a monad)
-}
loadTodos :: FilePath -> IO (Either String TodoList)
loadTodos path = do
    exists <- doesFileExist path
    if exists
        then do
            contents <- BS.readFile path
            -- eitherDecodeStrict returns Either String TodoList
            -- Left = parse error message, Right = successful parse
            pure $ eitherDecodeStrict contents
        else pure $ Right []  -- No file = empty list (not an error)

{- |
Save todos to a JSON file.

= ByteString vs String
- String = [Char] (linked list, inefficient for I/O)
- ByteString = compact byte array (efficient for I/O)
- Text = efficient Unicode text

Use ByteString/Text for real I/O, String only for simple cases.
-}
saveTodos :: FilePath -> TodoList -> IO ()
saveTodos path todos = BL.writeFile path (encode todos)
