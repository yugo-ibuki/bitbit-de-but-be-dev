{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Todo.Commands
Description : Command implementations for the Todo CLI

= Learning Points

== Higher-Order Functions
Functions that take functions as arguments or return functions.
filter, map, find are all higher-order functions.

== Pattern Matching
Haskell's way of destructuring data and handling different cases.
More powerful and safer than if-else chains.

== Partial Application
In Haskell, all functions are curried (take one argument at a time).
filter (\\t -> todoPriority t == High) todos
     ↑ This lambda is applied to each element

== Maybe Monad
Represents optional values. Safely handles missing data.
find returns Maybe because the element might not exist.
-}
module Todo.Commands
    ( addTodo
    , listTodos
    , completeTodo
    , deleteTodo
    , filterByPriority
    , filterByStatus
    , nextId
    ) where

import Data.List (find)
import Data.Text (Text)
import Data.Time (UTCTime)

import Todo.Types

{- |
Generate next available ID.

= Point-free Style
nextId = (+1) . maximum' where ...

Could be written as:
nextId todos = (+1) (maximum' todos)

Point-free removes the explicit parameter, using function composition (.)
-}
nextId :: TodoList -> TodoId
nextId [] = 1
nextId todos = 1 + maximum (map todoId todos)
-- maximum :: Ord a => [a] -> a  (requires non-empty list!)
-- map :: (a -> b) -> [a] -> [b]

{- |
Add a new todo item.

= Function Composition
Reading right to left: get ids -> find max -> add 1

= Record Creation
TodoItem { field1 = value1, field2 = value2, ... }
-}
addTodo :: Text -> Maybe Text -> Priority -> UTCTime -> TodoList -> TodoList
addTodo title desc priority now todos =
    let newTodo = TodoItem
            { todoId = nextId todos
            , todoTitle = title
            , todoDesc = desc
            , todoPriority = priority
            , todoStatus = Pending
            , todoCreatedAt = now
            }
    in newTodo : todos  -- (:) prepends to list, O(1) operation

{- |
List all todos (just returns them, formatting happens elsewhere).

This is intentionally simple - in functional programming, we separate
pure data transformation from I/O (printing).
-}
listTodos :: TodoList -> TodoList
listTodos = id  -- id :: a -> a (identity function, returns input unchanged)

{- |
Mark a todo as completed.

= map with Conditional Update
We transform the entire list, but only change matching items.
This is immutable: we create a NEW list, original is unchanged.

= Guards (|)
Like pattern matching but for boolean conditions.
-}
completeTodo :: TodoId -> TodoList -> TodoList
completeTodo targetId = map updateIfMatch
  where
    updateIfMatch todo
        | todoId todo == targetId = todo { todoStatus = Completed }
        | otherwise               = todo
    -- Record update syntax: todo { field = newValue }
    -- Creates a NEW record with one field changed

{- |
Delete a todo by ID.

= filter
filter :: (a -> Bool) -> [a] -> [a]
Keeps only elements where the predicate returns True.

= Lambda Functions
\\todo -> todoId todo /= targetId
Inline anonymous function. \\ is ASCII for λ (lambda).
-}
deleteTodo :: TodoId -> TodoList -> TodoList
deleteTodo targetId = filter (\\todo -> todoId todo /= targetId)
-- (/=) is "not equal" in Haskell (like != in other languages)

{- |
Filter todos by priority.

= Partial Application Example
filterByPriority High todos
is equivalent to:
filter (\\t -> todoPriority t == High) todos

The == High part is "baked in" to create a specialized filter.
-}
filterByPriority :: Priority -> TodoList -> TodoList
filterByPriority priority = filter (\\todo -> todoPriority todo == priority)

-- | Filter todos by status
filterByStatus :: Status -> TodoList -> TodoList
filterByStatus status = filter (\\todo -> todoStatus todo == status)

{- |
Find a todo by ID.

= Maybe Type
Maybe a = Nothing | Just a

find :: (a -> Bool) -> [a] -> Maybe a
Returns Nothing if no element matches, Just element if found.

This forces you to handle the "not found" case - no null pointer exceptions!
-}
findTodo :: TodoId -> TodoList -> Maybe TodoItem
findTodo targetId = find (\\todo -> todoId todo == targetId)
