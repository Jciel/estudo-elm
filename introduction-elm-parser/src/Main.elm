module Main exposing (..)

import Browser
import Html exposing (Html, div, text, br)
import Parser exposing (..)


type alias Phone =
    { countryCode : Maybe Int
    , areaCode : Maybe Int
    , phone : Int
    }


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')


-- countryCode : Parser Int
-- countryCode = 
--     succeed identity 
--        |. whitespace
--        |. symbol "+"
--        |= int
--        |. whitespace


-- Optional country code
countryCode : Parser (Maybe Int)
countryCode =
    oneOf
        [ succeed Just
            |. whitespace
            |. symbol "+"
            |= int
            |. whitespace
        , succeed Nothing
        ]


areaCode : Parser (Maybe Int)
areaCode =
    oneOf
    [ succeed String.toInt
        |. symbol "("
        |. whitespace
        |= (getChompedString <| chompWhile Char.isDigit)
        |. whitespace
        |. symbol ")"
        |. whitespace
    , succeed Nothing
    ]


localNumberStr : Parser String
localNumberStr =
    loop [] localHelp
        |> Parser.map String.concat


localHelp : List String -> Parser (Step (List String) (List String))
localHelp nums =
    let
        checkNum numsSoFar num =
            if String.length num > 0 then
                Loop (num :: numsSoFar)
            else
                Done (List.reverse numsSoFar)
    in
        succeed (checkNum nums)
            |= (getChompedString <| chompWhile Char.isDigit)
            |. whitespace


localNumber : Parser Int
localNumber =
    let
        checkDigits s =
            if String.length s == 7 then
                succeed s
            else
                problem "A NZ phone number has 7 digits"   
    in
        localNumberStr
            |> andThen checkDigits
            |> Parser.map String.toInt
            |> andThen
                (\maybe ->
                    case maybe of
                        Just n ->
                            succeed n
                            
                        Nothing ->
                            problem "Invalid local number"
                )


phoneParser : Parser Phone
phoneParser =
    succeed Phone
        |. whitespace
        |= countryCode
        |= areaCode
        |= localNumber








type alias Model =
    { input : String
    , phone : Result (List DeadEnd) Phone
    }


initialModel : Model
initialModel =
    { input = "+54 3333333"
    , phone = Err []
    }

type Msg
    = N

update : Msg -> Model -> Model
update msg model =
    model
=
view : Model -> Html Msg
view model =
    div []
        [
            text <| Debug.toString <| Parser.run phoneParser model.input
        ,   br [] []
        ,   text model.input
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }