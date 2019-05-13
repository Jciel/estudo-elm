module Main exposing (..)

import Browser exposing (element)
import Html exposing (div, button, text, h5, p, strong, ul)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import Http exposing (get)
import Json.Decode exposing (Decoder, list, string, map4, map7, field)



--
-- Model
--
type alias Model = 
    { listData: VisibleList }



type VisibleList = People (List Person)
                 | Planets (List Planet)


initialModel : Model
initialModel =
    People [] |> Model


listPeople : List Person
listPeople =
    []

listPlanets : List Planet
listPlanets =
    []

type alias Person =
    { name : String
    , height : String
    , mass : String
    , gender : String
    }

type alias Planet =
    { name : String
    , rotationPeriod : String
    , orbitalPeriod : String
    , diameter : String
    , climate : String
    , gravity : String
    , population : String
    }


--
-- Update
--
type Msg =  ApiCallLoadPeople (Result Http.Error (List Person))
          | ApiCallLoadPlanets (Result Http.Error (List Planet))
          | LoadPeople
          | LoadPlanets


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadPeople ->
            ( model, loadPeople )
        
        LoadPlanets ->
            ( model, loadPlanets )

        ApiCallLoadPeople (Ok people) ->
            let
                newModel = { model | listData = People people }
            in
                (newModel, Cmd.none)
        
        ApiCallLoadPlanets (Ok planets) ->
            let
                newModel = { model | listData = Planets planets }
            in
                ( newModel, Cmd.none )

        ApiCallLoadPeople (Err err) ->
            Debug.log (Debug.toString err)
            ( model, Cmd.none )

        ApiCallLoadPlanets (Err err) ->
            Debug.log (Debug.toString err)
            ( model, Cmd.none )

--
-- View
--
view : Model -> Html.Html Msg
view model = 
    div
        [ ]
        [ div
            [ class "mb-5 btn-group row" ]
            [ button
                [ class "btn btn-outline-primary", onClick LoadPeople]
                [ text "Listar pessoas" ]
            , button
                [ class "btn btn-outline-primary", onClick LoadPlanets ]
                [ text "Listar planetas" ]
            ]
        , div
            [ class "row" ]
            ( checkInfo model )
        ]

checkInfo : Model -> List (Html.Html Msg)
checkInfo model =
    case model.listData of
        People people ->
            Debug.log "people"
            (List.map cardPerson people)
    
        Planets planets ->
            Debug.log "planets"
            (List.map cardPlanet planets)


cardPerson : Person -> Html.Html Msg
cardPerson person =
    div
        [ class "card bg-info mr-3 mb-3", style "width" "14rem" ]
        [ div
            [ class "card-header" ]
            [ h5
                [ class "card-title" ]
                [ strong
                    []
                    [ text person.name ]
                ]
            ]
        , div
                [ class "card-body" ]
                [ p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Height: " ]
                    , text person.height
                    ]
                , p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Mass: " ]
                    , text person.mass
                    ]
                , p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Gender: " ]
                    , text person.gender
                    ]
                ]
        ]
        
cardPlanet : Planet -> Html.Html Msg
cardPlanet planet =
    div
        [ class "card bg-info mr-3 mb-3", style "width" "14rem" ]
        [ div
            [ class "card-header" ]
            [ h5
                [ class "card-title" ]
                [ strong
                    []
                    [ text planet.name ]
                ]
            ]
        , div
                [ class "card-body" ]
                [ p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Rotation period: " ]
                    , text planet.rotationPeriod
                    ]
                , p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Orbital period: " ]
                    , text planet.orbitalPeriod
                    ]
                , p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Diameter: " ]
                    , text planet.diameter
                    ]
                , p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Climate: " ]
                    , text planet.climate
                    ]
                , p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Gravity: " ]
                    , text planet.gravity
                    ]
                , p
                    [ class "card-text" ]
                    [ strong
                        []
                        [ text "Population: " ]
                    , text planet.population
                    ]
                ]
        ]




decodePerson : Decoder Person
decodePerson =
    map4 Person
        (field "name" Json.Decode.string)
        (field "height" Json.Decode.string)
        (field "mass" Json.Decode.string)
        (field "gender" Json.Decode.string)

decodePlanet : Decoder Planet
decodePlanet =
    map7 Planet
        (field "name" Json.Decode.string)
        (field "rotation_period" Json.Decode.string)
        (field "orbital_period" Json.Decode.string)
        (field "diameter" Json.Decode.string)
        (field "climate" Json.Decode.string)
        (field "gravity" Json.Decode.string)
        (field "population" Json.Decode.string)

peopleDecode : Decoder (List Person)
peopleDecode =
    field "results" (Json.Decode.list decodePerson)

planetsDecode : Decoder (List Planet)
planetsDecode =
    field "results" (Json.Decode.list decodePlanet) 


loadPeople =
    (Http.get
        { expect = Http.expectJson ApiCallLoadPeople peopleDecode
        , url = "https://swapi.co/api/people/" })

loadPlanets =
    (Http.get
        { expect = Http.expectJson ApiCallLoadPlanets planetsDecode
        , url = "https://swapi.co/api/planets/" })





init : () -> (Model, Cmd Msg)
init flags =
    (initialModel, Cmd.none)

main = Browser.element
    { init = (init)
    , update = update
    , subscriptions = (\n -> Sub.none)
    , view = view
    }
