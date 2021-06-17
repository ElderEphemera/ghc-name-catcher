module Foo.Bar where

import Data.List (intercalate)

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
