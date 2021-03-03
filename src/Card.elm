-----------------------
-- Stioiu Denis
-- 15.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (..)
import List exposing (..)
import Html exposing (..)
import Debug exposing(..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight |  Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type  Card = Card { face: Face,suit: Suit}

faceList = [Ace,Two,Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

faceToString: Face -> String

faceToString f =
    case f of
        Ace -> "Ace"
        Two -> "Two"
        Three -> "Three"
        Four -> "Four"
        Five -> "Five"
        Six -> "Six"
        Seven -> "Seven"
        Eight -> "Eight"
        Nine -> "Nice"
        Ten -> "Ten"
        Jack -> "Jack"
        Queen -> "Queen"
        King -> "King"

suitToString: Suit -> String

suitToString s =
    case s of
        Clubs -> "Clubs"
        Diamonds -> "Diamonds"
        Hearts -> "Hearts"
        Spades -> "Spades"

cardToString: Card -> String

cardToString (Card {face, suit}) = faceToString face ++ " of " ++ suitToString suit

cardValue : Card -> List Int
cardValue (Card {face,suit}) =
    case face of
        Two->[2]
        Three->[3]
        Four->[4]
        Five->[5]
        Six->[6]
        Seven->[7]
        Eight->[8]
        Nine->[9]
        Ten ->[10]
        Jack->[10]
        Queen->[10]
        King->[10]
        Ace->[1,11]


fromJust: Maybe a-> a
fromJust m =
    case m of
        Just a -> a
        Nothing -> Debug.todo ("result cannot be nothing")


suitDeck: Suit->List Face -> List Card
suitDeck s lf = if isEmpty lf /= True then [Card{face =(fromJust (head lf)) , suit = s}] ++ suitDeck s (drop 1 lf) else []

deck : List Card
deck = suitDeck Clubs faceList ++ suitDeck Diamonds faceList ++ suitDeck Spades faceList ++ suitDeck Hearts faceList


{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (Card {face ,suit}) =
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     Four -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard (Card {face ,suit}) =
   let
     card = (Card {face=face ,suit=suit})
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString card)]
     ]