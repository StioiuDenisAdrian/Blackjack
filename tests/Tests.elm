module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Main exposing (..)
import Card exposing (..)

genTest name fn input expected =
  test name <|
    \_ -> Expect.equal expected (fn input)

{-
  Uncomment the tests after you complete the `Card` module and implement the `calculateScore` function
-}
suite : Test
suite = describe "Test" [
   describe "calculateScore"
     [  genTest "calculateScore 1" calculateScore [Card {face=King,suit=Hearts}] 10
     ,  genTest "calculateScore 2" calculateScore [Card {face=Two,suit=Hearts}] 2
     ,  genTest "calculateScore 3" calculateScore [Card {face=Two, suit=Hearts}, Card {face=King,suit=Spades}] 12
     ,  genTest "calculateScore 4" calculateScore [Card {face=Ace,suit=Hearts}, Card {face=King, suit=Spades}] 21
     ,  genTest "calculateScore 5" calculateScore [Card {face=Ace,suit=Hearts}, Card {face=Five,suit=Hearts}, Card {face=Seven,suit=Spades}] 13
     ,  genTest "calculateScore 6" calculateScore [Card {face=King,suit=Hearts}, Card {face=Five,suit=Hearts}, Card {face=Seven,suit=Spades}] 22
     ,  genTest "calculateScore 7" calculateScore [Card {face=King, suit=Hearts}, Card {face=Ten,suit=Clubs}, Card {face=Ace, suit=Spades}] 21
     ,  genTest "calculateScore 8" calculateScore [Card {face=Ace, suit=Spades}, Card {face=Ace,suit=Clubs}, Card {face=Ten, suit=Clubs}, Card{face=King,suit=Clubs}] 22
     ]
  ]

