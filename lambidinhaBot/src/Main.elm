module Main exposing (..)

import Browser exposing (element)
import Http exposing (..)
import Html exposing (..)
import Json.Decode exposing (Decoder, list, field, string, map)

type alias Joke =
    {joke : String}

type alias Model =
    {jokes : List Joke}

initialModel : Model
initialModel =
    Model []


type Msg = LoadJokes 
         | ApiCall (Result Http.Error (List Joke))
    

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadJokes ->
            (model, loadJokes)

        ApiCall result ->
            case result of
                Ok jokes ->
                    ({model | jokes = jokes}, Cmd.none)
            
                Err error ->
                    (model, Cmd.none)

view : Model -> Html.Html Msg
view model =
        div
            []
            [ ul [] (List.map viewJoke model.jokes) ]

viewJoke : Joke -> Html.Html Msg
viewJoke joke =
    li
        []
        [text joke.joke]


jokeDecode : Decoder Joke
jokeDecode =
    map Joke
        (field "joke" string)

decode : Decoder (List Joke)
decode =
    (Json.Decode.list jokeDecode)

loadJokes =
        (Http.get
            { expect = Http.expectJson ApiCall decode
            , url= "https://raw.githubusercontent.com/lambda-study-group/lambdinha-bot/master/jokes.json"
            })


init : () -> (Model, Cmd Msg)
init flags =
    (initialModel, loadJokes)

main =  Browser.element
       { init = (init)
       , update = update
       , subscriptions = (\n -> Sub.none)
       , view = view
       }
