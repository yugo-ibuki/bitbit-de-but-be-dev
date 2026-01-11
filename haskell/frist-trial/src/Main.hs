{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Entry point for the Todo CLI application

= Learning Points

== IO Monad in Practice
main :: IO () means this function performs side effects and returns nothing.
All user interaction, file I/O happens here.

== case Expressions
Pattern matching as an expression (returns a value).
Different from case statements in other languages - this is exhaustive matching.

== Text vs String
We use Data.Text for efficiency. pack converts String to Text.
-}
module Main where

import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Todo.Types
import Todo.Storage
import Todo.Commands

-- | Main entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["add"]      -> runAdd
        ["list"]     -> runList
        ["complete", idStr] -> runComplete (read idStr)
        ["delete", idStr]   -> runDelete (read idStr)
        ["help"]     -> printHelp
        _            -> printHelp

{- |
= do-notation Desugaring

This:
@
do
    result <- loadTodos defaultFilePath
    case result of ...
@

Is syntactic sugar for:
@
loadTodos defaultFilePath >>= \\result ->
    case result of ...
@

(>>=) is called "bind" - chains monadic operations.
-}
runList :: IO ()
runList = do
    result <- loadTodos defaultFilePath
    case result of
        Left err -> putStrLn $ "Error loading todos: " ++ err
        Right todos -> mapM_ printTodo todos
  where
    printTodo todo = putStrLn $ formatTodo todo

{- |
Format a todo for display.

= String Interpolation in Haskell
Haskell doesn't have string interpolation like "Hello, {name}".
We concatenate with (++) or use formatting libraries.

= show
show :: Show a => a -> String
Converts any showable value to a String.
-}
formatTodo :: TodoItem -> String
formatTodo todo = unwords
    [ "[" ++ show (todoId todo) ++ "]"
    , "[" ++ show (todoStatus todo) ++ "]"
    , "[" ++ show (todoPriority todo) ++ "]"
    , show (todoTitle todo)
    ]

-- | Interactive todo addition
runAdd :: IO ()
runAdd = do
    putStr "Title: "
    hFlush stdout  -- Ensure prompt shows before input
    title <- getLine

    putStr "Description (optional, press Enter to skip): "
    hFlush stdout
    descInput <- getLine
    let desc = if null descInput then Nothing else Just (pack descInput)

    putStr "Priority (high/medium/low): "
    hFlush stdout
    priorityInput <- getLine
    let priority = parsePriority priorityInput

    now <- getCurrentTime
    result <- loadTodos defaultFilePath
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right todos -> do
            let newTodos = addTodo (pack title) desc priority now todos
            saveTodos defaultFilePath newTodos
            putStrLn "Todo added!"

{- |
Parse priority from user input.

= Pattern Matching on Strings
Strings are [Char], so we can pattern match like any list.
Using guards here for clarity.

= Default Case
'_' matches anything - ensures we handle all cases.
Without this, we'd get a warning about non-exhaustive patterns.
-}
parsePriority :: String -> Priority
parsePriority input = case input of
    "high"   -> High
    "h"      -> High
    "medium" -> Medium
    "m"      -> Medium
    "low"    -> Low
    "l"      -> Low
    _        -> Medium  -- Default to medium

runComplete :: TodoId -> IO ()
runComplete targetId = do
    result <- loadTodos defaultFilePath
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right todos -> do
            let updatedTodos = completeTodo targetId todos
            saveTodos defaultFilePath updatedTodos
            putStrLn $ "Completed todo #" ++ show targetId

runDelete :: TodoId -> IO ()
runDelete targetId = do
    result <- loadTodos defaultFilePath
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right todos -> do
            let updatedTodos = deleteTodo targetId todos
            saveTodos defaultFilePath updatedTodos
            putStrLn $ "Deleted todo #" ++ show targetId

printHelp :: IO ()
printHelp = putStrLn $ unlines
    [ "Todo CLI - A Haskell learning project"
    , ""
    , "Usage:"
    , "  todo-cli add              Add a new todo (interactive)"
    , "  todo-cli list             List all todos"
    , "  todo-cli complete <id>    Mark todo as completed"
    , "  todo-cli delete <id>      Delete a todo"
    , "  todo-cli help             Show this help"
    ]
