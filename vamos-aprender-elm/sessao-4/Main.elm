module Main exposing (..)

import Html exposing (br, button, div, form, h1, h3, input, label, li, strong, ul, text, textarea)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (value, class)
import Http exposing (..)
import Task exposing (..)
import Browser exposing (element)
import Json.Decode exposing (Decoder, list, field, nullable, string, map2)
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode exposing (..)
import String
import Avatar exposing (..)

import Iso8601 exposing (..)
import Date exposing (..)
import Parser exposing (..)
import Time exposing (..)
import DateFormat exposing (..)

type alias Comment =
    { author : String
    , content : String
    , email : Avatar.Model
    , date : String
    , saved : Bool
    }

type alias Model =
    { comments : List Comment
    , form : Comment
    , loaded : Bool
    }


initialModel : Model
initialModel =
    Model [] (Comment "" "" Avatar.model "" False) False



--
-- Update
--
type Msg = PostComment
         | UpdateAuthor String
         | UpdateContent String
         | LoadComments
         | ApiSuccess (List Comment)
         | ApiCall (Result Http.Error (List Comment))
         | RessetForm
         | ApiPost (Result Http.Error Comment)
         | AvatarMessage Avatar.Msg
         | UpdateEmail String



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadComments ->
            (model, loadComments)

        ApiSuccess comments ->
            ( resetForm model, Cmd.none)

        ApiCall (Ok comments) ->
            let
                modell = { model | comments = comments , loaded = True }
            in
                (modell, Cmd.none)
        
        ApiCall (Err error) ->
            (model, Cmd.none)

        ApiPost (Ok comment) ->
            let
                newComments = updateCommentsStatus comment model.comments
                modell = { model | comments = newComments , loaded = True }
            in
                (modell, Cmd.none)
        
        ApiPost (Err error) ->
            (model, Cmd.none)

        RessetForm ->
            ( resetForm model, Cmd.none)

        PostComment ->
            let
                author = model.form.author |> String.trim
                content = model.form.content |> String.trim
                modell = 
                    if String.isEmpty author && String.isEmpty content then
                        model
                    else
                        { model | comments = List.append model.comments [ model.form ] }
            in
                ( resetForm modell, saveComment model.form )
            
        UpdateAuthor author ->
            let
                form = model.form
                newForm = { form | author = author }
            in
                ({ model | form = newForm }, Cmd.none)

        UpdateContent content ->
            let
                form = model.form
                newForm = { form | content = content }
            in
                ({ model | form = newForm }, Cmd.none)

        AvatarMessage avatarMessage ->
            ( model, Cmd.none )

        UpdateEmail email ->
            let
                form = model.form
                newForm = { form | email = Just email }
            in
                ( { model | form = newForm }, Cmd.none )
            




updateCommentStatus : Comment -> Comment -> Comment
updateCommentStatus new current =
    if current.author == new.author && current.content == new.content then
        { current | saved = True }
    else
        current

updateCommentsStatus : Comment -> List Comment -> List Comment
updateCommentsStatus newComment comments =
    List.map (updateCommentStatus newComment) comments

-- decodeComment : Decoder Comment
-- decodeComment =require
--     map2 Comment
--         (field "author" Json.Decode.string)
--         (field "content" Json.Decode.string)

--
-- Decoder
--
decodeComment : Decoder Comment
decodeComment =
    Json.Decode.succeed Comment
        |> required "author" Json.Decode.string
        |> required "content" Json.Decode.string
        |> required "email" (Json.Decode.nullable Json.Decode.string)
        |> required "date" Json.Decode.string
        |> hardcoded True

decode : Decoder (List Comment)
decode =
    field "comments" (Json.Decode.list decodeComment)

loadComments =
    (Http.get
        { expect = Http.expectJson ApiCall decode
        , url= "http://vamosaprenderelm.herokuapp.com/api/comments"
        })

saveComment : Comment -> Cmd Msg
saveComment comment =
    let
        email =
            case comment.email of
                Just emaill ->
                    emaill
            
                Nothing ->
                    ""
 
        data =
            Json.Encode.object
                [ ("author", Json.Encode.string comment.author )
                , ("content", Json.Encode.string comment.content )
                , ("email", Json.Encode.string email )
                ]
    in
        (Http.post
            { expect = Http.expectJson ApiPost decodeComment
            , url = "http://vamosaprenderelm.herokuapp.com/api/comments/"
            , body = Http.jsonBody data
            })

--
-- View
--
view : Model -> Html.Html Msg
view model =
    if model.loaded then
        let
            count = List.length model.comments

            title = String.fromInt count ++ pluralize " Comentário" count
        in
            div
                []
                [ h3 [] [ Html.text title ]
                , ul [ class "list-unstyled" ] (List.map viewComment model.comments)
                , Html.form
                    [ onSubmit PostComment ]

                    [ div 
                        [ class "form-group" ]
                        [ label [] [ Html.text "Nome:" ]
                        , input
                            [ class "form-control"
                            , onInput UpdateAuthor
                            , value model.form.author
                            ]
                            []
                        , label [] [ Html.text "E-Mail:" ]
                        , input
                            [ class "form-control"
                            , onInput UpdateEmail
                            , value <| Maybe.withDefault "" model.form.email
                            ]
                            []
                        ]
                    , div
                        [ class "form-group" ]
                        [ label [] [ Html.text "Comentario:" ]
                        , textarea
                            [ class "form-control"
                            , onInput UpdateContent
                            , value model.form.content
                            ]
                            []
                        ]
                    , button [ class "btn btn-primary" ] [ Html.text "Enviar" ]
                    , button [ class "btn btn-default" ] [ Html.text "Ressetar" ]
                ]
            ]
                    
                
    else
         h1 [] [ Html.text "Loading..." ]


resetForm : Model -> Model
resetForm model =
    { model | form = Comment "" "" Avatar.model "" False}

pluralize : String -> Int -> String
pluralize name count =
    if count == 1 then
        name
    else
        name ++ "s"


teste comment =
    let
        posixResult = Iso8601.toTime comment.date
    in
        case posixResult of
            Ok posix ->
                DateFormat.format 
                    [ DateFormat.dayOfMonthNumber
                    , DateFormat.text "/"
                    , DateFormat.monthNumber
                    , DateFormat.text "/"
                    , DateFormat.yearNumber
                    ]
                    utc
                    posix
                    
            Err er ->
                ""
                

viewComment : Comment -> Html.Html Msg
viewComment comment =
    let
        commentClass =
            if comment.saved then
                ""
            else
                "saving"

        avatar = 
            Html.map
                (\msg -> AvatarMessage msg)
                (Avatar.view comment.email)

        date = teste comment
   
    in
        li
            [ class commentClass ]
            [ avatar
            , br [] []
            , strong [] [ Html.text comment.author ]
            , br [] []
            , Html.text comment.content
            , br [] []
            , Html.text date
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





