module Main exposing (..)

import Html exposing (br, button, div, form, h1, input, li, strong, ul, text, textarea)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (value)
import Http exposing (..)
import Task exposing (..)
import Browser exposing (element)
import Json.Decode exposing (Decoder, list, field, string, map2)


type alias Comment =
    { author : String
    , content : String
    }

type alias Model =
    { comments : List Comment
    , form : Comment
    }


initialModel : Model
initialModel =
    Model [] <| Comment "" ""



--
-- Update
--
type Msg = PostComment
         | UpdateAuthor String
         | UpdateContent String
         | LoadComments
         | ApiSuccess (List Comment)
         | ApiFail (Result Http.Error (List Comment))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadComments ->
            (model, loadComments)

        ApiSuccess comments ->
            ({model | comments = comments}, Cmd.none)

        ApiFail result ->
            case result of
                Ok comments ->
                    ({model | comments = comments}, Cmd.none)
            
                Err _ ->
                    (model, Cmd.none)

        PostComment ->
            let
                modell = 
                    { comments = List.append model.comments [ model.form ]
                    , form = Comment "" ""
                    }
            in
                (modell, Cmd.none)
            
        UpdateAuthor value ->
            ({ model | form = Comment value model.form.content }, Cmd.none)

        UpdateContent value ->
            ({ model | form = Comment model.form.author value }, Cmd.none)


--
-- Decoder
--
decodeComment : Decoder Comment
decodeComment =
    map2 Comment
        (field "author" string)
        (field "content" string)

decode : Decoder (List Comment)
decode =
    field "comments" (Json.Decode.list decodeComment)

loadComments =
    -- Task.perform
        -- ApiSuccess
        (Http.get
            { expect = Http.expectJson ApiFail decode
            , url= "http://vamosaprenderelm.herokuapp.com/api/comments"
            })
--
-- View
--
view : Model -> Html.Html Msg
view model =
    let
        count = List.length model.comments

        title = String.fromInt count ++ pluralize " Comentário" count
    in
        div
            []
            [ h1 [] [ text title ]
            , ul [] (List.map viewComment model.comments)
            , Html.form
                [ onSubmit PostComment ]
                [ text "Nome:"
                , br [] []
                , input [ onInput UpdateAuthor, value model.form.author ] []
                , br [] []
                , text "Comentário:"
                , br [] []
                , textarea [ onInput UpdateContent, value model.form.content ] []
                , br [] []
                , button [] [ text "Enviar" ]
                ]
            ]


pluralize : String -> Int -> String
pluralize name count =
    if count == 1 then
        name
    else
        name ++ "s"


viewComment : Comment -> Html.Html Msg
viewComment comment =
    li
        []
        [ strong [] [ text comment.author ]
        , br [] []
        , text comment.content
        ]

init : () -> (Model, Cmd Msg)
init flags =
    (initialModel, loadComments)

main =  Browser.element
       { init = (init)
       , update = update
       , subscriptions = (\n -> Sub.none)
       , view = view
       }





