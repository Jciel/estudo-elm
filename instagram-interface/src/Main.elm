module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (Decoder, list, string, map, map2, map5, field, value, lazy)
import Browser
import Http
import String
import Array


type alias Storie =
    { urlImage : String
    }

type alias User =
    { name : String
    , nickName : String
    , profileImgUrl : String
    , stories : List Storie
    , posts : List Post
    }


type alias Comment =
    { user : Int
    , content : String
    }

type alias Post =
    { user: Int
    , urlImage : String
    , title : String
    , likes : Maybe (List Int)
    , comments : List Comment
    }

type alias UserData =
    { following : List User
    }

type Model = ModelUserData UserData
           | Loading


type Msg
  = InitPage (Result Http.Error (List User))



decodeUser : Decoder User
decodeUser =
    map5 User
        (field "name" Json.Decode.string)
        (field "nickname" Json.Decode.string)
        (field "profileImgUrl" Json.Decode.string)
        (field "stories" (Json.Decode.list decodeStorie))
        (field "posts" (Json.Decode.list decodePost))


decodePost : Decoder Post
decodePost =
    map5 Post
        (field "user" Json.Decode.int)
        (field "urlImage" Json.Decode.string)
        (field "title" Json.Decode.string)
        (field "likes" (Json.Decode.maybe (Json.Decode.list Json.Decode.int)))
        (field "comments" (Json.Decode.list decodeComment))


decodeStorie : Decoder Storie
decodeStorie =
    map Storie
        (field "urlImg" Json.Decode.string)

decodeComment : Decoder Comment
decodeComment =
    map2 Comment
        (field "user" Json.Decode.int)
        (field "content" Json.Decode.string)

userDecode : Decoder (List User)
userDecode =
    field "following" (Json.Decode.list decodeUser)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InitPage (Ok result) ->
--            Debug.log (Debug.toString result)
            (ModelUserData <| UserData result, Cmd.none)

        InitPage (Err err) ->
            Debug.log (Debug.toString err)
            (model, Cmd.none)



view : Model -> Html.Html Msg
view model =
    case model of
        Loading ->
            text "hi"

        ModelUserData userData->
            section
                [ class "post-section"]
                [ div
                    [ class "post-header"]
                    [ div
                        [ class "profile-image"]
                        [ img
                            [ src "/bd/images/rosinha-do-sol/02.jpg"]
                            []
                        ]
                    , div
                        [ class "profile-nickname"]
                        [ text "Jociel souza"]
                    ]
                , div
                    [ class "image-post-container" ]
                    [ img [ src ""]
                          []
                    ]
                , div
                    [ class "comments-container" ]
                    []
                , div [ class "comments-container" ]
                      []
                ]









userById : List User -> Int -> Maybe User
userById following id =
    Array.get id <| Array.fromList following






init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
    { expect = Http.expectJson InitPage userDecode
    , url = "../bd/bd2.json" })

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = (\n -> Sub.none)
    , view = view
    }