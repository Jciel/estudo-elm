module Main exposing (main)

import Html exposing (text)
import Json.Decode exposing (Decoder, list, string, map, map2, map5, field, value, lazy)
import Browser
import Http
import String


type alias Storie =
    { urlImage : String
    }

type alias User =
    { name : String
    , nickName : String
    , profileImgUrl : String
    , stories : List Storie
    , posts : ListPosts
    }


type alias Comment =
    { user : Maybe User
    , content : String
    }

type alias Post =
    { user : Maybe User
    , urlImage : String
    , title : String
    , likes : List (Maybe User)
    , comments : List Comment
    }

type ListPosts = ListPosts (List Post)


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
        (field "posts" (map ListPosts (Json.Decode.list decodePost)))


decodePost : Decoder Post
decodePost =
    map5 Post
        (field "user" (Json.Decode.lazy (\_ -> Json.Decode.maybe decodeUser)))
        (field "urlImage" Json.Decode.string)
        (field "title" Json.Decode.string)
        (field "likes" (Json.Decode.list (Json.Decode.lazy (\_ -> Json.Decode.maybe decodeUser))))
        (field "comments" (Json.Decode.list decodeComment))


decodeStorie : Decoder Storie
decodeStorie =
    map Storie
        (field "urlImg" Json.Decode.string)

decodeComment : Decoder Comment
decodeComment =
    map2 Comment
        (field "user" (Json.Decode.lazy (\_ -> Json.Decode.maybe decodeUser)))
        (field "content" Json.Decode.string)

userDecode : Decoder (List User)
userDecode =
    field "following" (Json.Decode.list decodeUser)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InitPage (Ok result) ->
            Debug.log (Debug.toString result)
            (model, Cmd.none)

        InitPage (Err err) ->
            Debug.log (Debug.toString err)
            (model, Cmd.none)



view : Model -> Html.Html Msg
view model =
    text "hi"


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
    { expect = Http.expectJson InitPage userDecode
    , url = "../bd/bd.json" })

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = (\n -> Sub.none)
    , view = view
    }