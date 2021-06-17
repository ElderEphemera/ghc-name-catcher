{-# OPTIONS_GHC -fplugin=GhcNameCatcher -fplugin-opt=GhcNameCatcher:./out #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Data.List (intercalate)

import Test.HUnit

-- TODO: Write a more comprehensive test suite
main :: IO ()
main = do
  csv <- readFile "out/Main.csv"
  runTestTTAndExit $
    "Main.csv"
    ~: csv
    ~=? shouldBe

shouldBe :: String
shouldBe = unlines $ map (intercalate ",")
  [ ["Foo", "barFoo", "foo"]
  , ["IO", "main"]
  , ["Int", "luckyInt", "n"]
  , ["Possibly", "couldBe5", "k", "noWay", "poss"]
  , ["String", "csv", "shouldBe"]
  ]

luckyInt :: Int
luckyInt = 7

data Foo = Bar | Baz

data Possibly a = Nope | Kinda a | Definitely a

barFoo :: Foo
barFoo = Bar

noWay :: Possibly a
noWay = Nope

couldBe5 :: Possibly Int
couldBe5 = Kinda 5

makeItNow :: Foo -> Int -> Possibly a -> Maybe a
makeItNow Bar n = \_ -> Nothing
makeItNow foo n = \poss -> case poss of
  Nope -> Nothing
  k@(Kinda miss) -> makeItNow Bar 0 k
  Definitely again -> n `seq` Just again
