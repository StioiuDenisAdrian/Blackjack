-----------------------
-- Stioiu Denis
-- 15.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Random
import Debug


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToogleDeck
  | ResetGame


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ({model | hand = newCard::model.hand, deck = List.filter (\x->x/=newCard) model.deck }
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      (
       {model | showDeck = not model.showDeck}
      , Cmd.none
      )
    ResetGame ->
        (
        startingModel,Cmd.none
        )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hears, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hears, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}
calculateScore : List Card -> Int
calculateScore cards =
    let
        --We make a list only with Aces from the hand
        aceList = List.filter (\x->String.contains  "Ace" (cardToString x)) cards
        --We make a list with the other cards from the hand
        otherCards = List.filter (\x->String.contains  "Ace" (cardToString x)==False) cards
        --We calculate the sum of the hand that cannot change, because with the exception of Aces of cards have fixed value
        sumWithoutAces =List.sum (List.concat(List.map cardValue otherCards))
        --We make a function which creates the a list with the possible values of a sum if it has an Ace
        calculateAces: Card-> List Int->List Int
        calculateAces c sum =
            let
                plus1=
                    List.map (\s->s+1) sum
                plus11=
                    List.map (\s->s+11) sum
            in
                plus1++plus11
        head l =
                case l of
                    []-> 0
                    x::xs->x
        --We calculate all the possible scores of the hand with the Aces and we store them in a list by applying a fold on all the ace list and sorting them in decreasing order to help us getting the right score easier
        possibleScores =List.sortWith (\a b-> compare b a) (List.foldl calculateAces [sumWithoutAces] aceList)
    in
        --if a possibility of score is 21 (in case we have aces in hand), then the game is won
        if List.member 21 possibleScores == True then 21
        --if there is no possible score smaller than 21 then the game is lost and we take the smallest score under 21 from the list
        else if List.filter (\x->x<21)  possibleScores == [] then head (List.reverse (possibleScores))
        --else we keep the biggest number from the list under 21
        else head (List.filter(\x->x<21) possibleScores)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
    d =
        model.deck
        |> List.map Card.cardToUnicode
        |> List.intersperse " "
        |> List.map text
    h =
         model.hand
                |> List.reverse
                |> List.map Card.viewCard
    score =
       calculateScore model.hand

    gameStatus= if score == 21 then "Game Won" else if score > 21 then "Game Lost" else ""
  in
    div []
      [ div [] [ h1 [] [text appName] ],
        div [] [
            button [onClick Draw , if gameStatus == "Game Won" || gameStatus == "Game Lost" then disabled True else disabled False ] [text "Draw a card"],
            button [onClick ToogleDeck] [text "Toggle"],
            button [onClick ResetGame] [text "Reset game"]
        ],
        div [style "font-size" "1em"] h,
        div [style "font-size" "2em"]  [text ("Score: "++String.fromInt score)],
        div [style "font-size" "3em"]  [text gameStatus],
        div [style "font-size" "6em", if model.showDeck==True then style "display" "" else style "display" "none"] d
      ]

