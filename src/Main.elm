module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder)


type alias Model =
    { items : List Item }


type alias Item =
    { who : String
    , title : String
    , dueInDays : Int
    , status : String
    , renewed : Maybe String
    , image : Maybe ItemImage
    }


type alias ItemImage =
    { thumb : String
    , normal : String
    }


initialModel : Model
initialModel =
    { items = [] }


type Msg
    = Fetch


update : Msg -> Model -> Model
update msg model =
    case msg of
        Fetch ->
            let
                items =
                    case JD.decodeString (JD.field "results" (JD.list itemDecoder)) data of
                        Ok i ->
                            i

                        Err _ ->
                            []
            in
            { model | items = List.sortBy .status items }


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ button [ class "btn btn-primary", onClick Fetch ] [ text "Fetch" ]
        , div [] (List.map viewItem model.items)
        ]


viewItem : Item -> Html Msg
viewItem item =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "card border" ]
                [ viewCardImage item.image
                , div [ class "card-body" ]
                    [ h5 [ class "card-title" ] [ text item.title ]
                    , h6 [ class "card-subtitle" ] [ text item.who ]
                    ]
                , div [ class "card-body" ]
                    (List.concat
                        [ Maybe.withDefault [] (Maybe.map (\s -> [ div [ class "alert alert-secondary" ] [ text s ] ]) item.renewed)
                        , [ div [ class "alert alert-primary" ] [ text item.status ] ]
                        ]
                    )
                ]
            ]
        ]


viewCardImage : Maybe ItemImage -> Html Msg
viewCardImage image =
    case image of
        Nothing ->
            text ""

        Just url ->
            img [ src url.normal, class "card-img-top" ] []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


itemDecoder : Decoder Item
itemDecoder =
    JD.map6 Item
        (JD.field "who" JD.string)
        (JD.field "title" JD.string)
        (JD.field "dueInDays" JD.int)
        (JD.field "status" JD.string)
        (JD.field "renewed" (JD.maybe JD.string))
        (JD.field "image" (JD.maybe itemImageDecoder))


itemImageDecoder : Decoder ItemImage
itemImageDecoder =
    JD.map2 ItemImage
        (JD.field "thumb" JD.string)
        (JD.field "normal" JD.string)


data =
    """
REDACTED
"""
