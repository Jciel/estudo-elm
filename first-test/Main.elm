module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (..)
import Browser exposing (sandbox)



type alias Comment =
    {
        author : String,
        contents : String
    }

type Msg = UpdateAuthor String | UpdateContents String | PostComment

type alias Model =
    {
        new : Comment,
        comments : List Comment
    }

initialModel : Model
initialModel = 
    {
        new = {
            author = "",
            contents = ""
        },
        comments = []
    }


view : Model -> Html.Html Msg
view model =
    let
        count =
            List.length model.comments
        
        phrase =
            (String.fromInt count) ++ pluralize count
    
    in
        div
            []
            [ p [] [ text phrase ],
            div [] (List.map viewComment model.comments),
            Html.form
                [onSubmit PostComment]
                [ input [ value model.new.author, onInput UpdateAuthor ] [],
                br [] [],
                textarea [ value model.new.contents, onInput UpdateContents ] [],
                br [] [],
                button [] [ text "Enviar" ]
                ]
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateAuthor value ->
            let
                new =
                    model.new

                updated =
                    { new | author = value }

            in
                { model | new = updated }

        UpdateContents value ->
            let
                new =
                    model.new

                updated =
                    { new | contents = value }

                in
                    { model | new = updated }

        PostComment ->
            let
                comments =
                    List.append model.comments [ model.new ]

            in
                { model | new = Comment "" "", comments = comments }



pluralize : Int -> String
pluralize count =
    if count == 1 then
        " comentário"
    else
        " comentários"


viewComment : Comment -> Html.Html a
viewComment comment =
    p
        []
        [ text (comment.author ++ ":"),
        br [] [],
        text comment.contents
        ]


main : Program () Model Msg
main =  Browser.sandbox
       { init = initialModel
       , view = view
        , update = update}