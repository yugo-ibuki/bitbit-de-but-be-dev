{-# LANGUAGE OverloadedStrings #-}

{- |
= Testing in Haskell with HSpec

HSpec uses a describe/it pattern similar to RSpec/Jest.
Pure functions are especially easy to test - no mocking needed!
-}
module Todo.TypesSpec (spec) where

import Test.Hspec
import Data.Time (UTCTime(..))
import Data.Time.Calendar (fromGregorian)

import Todo.Types
import Todo.Commands

-- | Test fixture: a sample time for tests
sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 1) 0

-- | Test fixture: sample todos
sampleTodos :: TodoList
sampleTodos =
    [ TodoItem 1 "Learn Haskell" Nothing High Pending sampleTime
    , TodoItem 2 "Build project" (Just "Todo CLI app") Medium InProgress sampleTime
    , TodoItem 3 "Write tests" Nothing Low Completed sampleTime
    ]

spec :: Spec
spec = do
    describe "nextId" $ do
        it "returns 1 for empty list" $ do
            nextId [] `shouldBe` 1

        it "returns max + 1 for non-empty list" $ do
            nextId sampleTodos `shouldBe` 4

    describe "addTodo" $ do
        it "adds a new todo with correct ID" $ do
            let newTodos = addTodo "New task" Nothing Medium sampleTime sampleTodos
            length newTodos `shouldBe` 4
            todoId (head newTodos) `shouldBe` 4

    describe "deleteTodo" $ do
        it "removes the todo with matching ID" $ do
            let result = deleteTodo 2 sampleTodos
            length result `shouldBe` 2
            filter (\t -> todoId t == 2) result `shouldBe` []

    describe "completeTodo" $ do
        it "marks the correct todo as completed" $ do
            let result = completeTodo 1 sampleTodos
            let updated = head $ filter (\t -> todoId t == 1) result
            todoStatus updated `shouldBe` Completed

    describe "filterByPriority" $ do
        it "returns only todos with matching priority" $ do
            let highPriority = filterByPriority High sampleTodos
            length highPriority `shouldBe` 1
            todoPriority (head highPriority) `shouldBe` High

    describe "filterByStatus" $ do
        it "returns only todos with matching status" $ do
            let completed = filterByStatus Completed sampleTodos
            length completed `shouldBe` 1
