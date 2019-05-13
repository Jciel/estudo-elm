module Main exposing (..)

import Html exposing (div, ul, li, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String
import Browser exposing (sandbox)




type alias Model =
    {
        counter : Int,
        clicks : Int
    }

initialModel : Model
initialModel =
    {
        counter = 0,
        clicks = 0
    }


type Msg = Increment | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            {
                counter = model.counter + 1,
                clicks = model.clicks + 1
            }

        Decrement ->
            {
                counter = model.counter - 1,
                clicks = model.clicks + 1
            }


view : Model -> Html.Html Msg
view model =
    div
        []
        [ button [ onClick Decrement ] [ text "-" ]
        , text <| String.fromInt model.counter
        , button [ onClick Increment ] [ text "+" ]
        , text <| String.fromInt model.clicks ++ " clicks"
        ]




--    Html.text <| String.fromInt num




main =  Browser.sandbox
       {
           init = initialModel,
           update = update,
           view = view
       }