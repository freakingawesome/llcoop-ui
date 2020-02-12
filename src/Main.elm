module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import RemoteData exposing (WebData)


type alias Model =
    { items : WebData (List Item)
    , settings : WebData Settings
    , page : Page
    }


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


type Msg
    = Fetch
    | LoadSettings
    | GotItems (Result Http.Error (List Item))
    | GoTo Page
    | AppendCredentials
    | SetCredentialField Int CredentialField String


type Page
    = ItemList
    | SettingsEdit


type alias Settings =
    { credentials : List Credential }


type alias Credential =
    { rootUrl : String
    , tz : String
    , name : String
    , barcode : String
    , pin : String
    }


type CredentialField
    = RootUrl
    | Tz
    | Name
    | Barcode
    | Pin


init : Settings -> ( Model, Cmd Msg )
init settings =
    ( { items = RemoteData.Loading
      , settings = RemoteData.Success settings
      , page = ItemList
      }
    , fetchItems settings
    )


fetchItems : Settings -> Cmd Msg
fetchItems settings =
    Http.get
        { url = "/example-response.json"
        , expect = Http.expectJson GotItems (JD.field "results" (JD.list itemDecoder))
        }


fetchItemsREAL : Settings -> Cmd Msg
fetchItemsREAL settings =
    Http.post
        { url = "/api/library/items"
        , body = Http.jsonBody (encodeSettings settings)
        , expect = Http.expectJson GotItems (JD.field "results" (JD.list itemDecoder))
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadSettings ->
            ( model, Cmd.none )

        GotItems (Err httpError) ->
            Debug.log (Debug.toString httpError) ( model, Cmd.none )

        GotItems (Ok items) ->
            ( { model | items = RemoteData.Success (List.sortBy .dueInDays items) }, Cmd.none )

        GoTo pg ->
            ( { model | page = pg }, Cmd.none )

        Fetch ->
            ( model, Cmd.none )

        AppendCredentials ->
            case model.settings of
                RemoteData.Success settings ->
                    let
                        newSettings =
                            { settings | credentials = List.append settings.credentials [ emptyCredential ] }
                    in
                    ( { model | settings = RemoteData.Success newSettings }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetCredentialField index field value ->
            ( model, Cmd.none )


emptyCredential : Credential
emptyCredential =
    { rootUrl = "https://sam.llcoop.org"
    , tz = "US/Eastern"
    , name = ""
    , barcode = ""
    , pin = ""
    }



{-
   let
       items =
           case JD.decodeString (JD.field "results" (JD.list itemDecoder)) data of
               Ok i ->
                   i

               Err _ ->
                   []
   in
   ( { model | items = List.sortBy .status items }, Cmd.none )
-}


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ viewNav model.page
        , if model.page == ItemList then
            viewItems model.items

          else
            div [] []
        , if model.page == SettingsEdit then
            viewSettingsData model.settings

          else
            div [] []
        ]


viewNav : Page -> Html Msg
viewNav page =
    let
        links =
            [ ( ItemList, "Checked-Out" ), ( SettingsEdit, "Settings" ) ]

        navlink ( pg, title ) =
            li
                [ class
                    ("nav-item"
                        ++ (if page == pg then
                                " active"

                            else
                                ""
                           )
                    )
                ]
                [ a [ class "nav-link", style "cursor" "pointer", onClick (GoTo pg) ] [ text title ] ]
    in
    div [ class "navbar navbar-expand-lg navbar-light bg-light" ]
        [ div [ class "collapse navbar-collapse " ]
            [ ul [ class "navbar-nav" ] (List.map navlink links)
            ]
        ]


viewItems : WebData (List Item) -> Html Msg
viewItems itemsWrapper =
    case itemsWrapper of
        RemoteData.Success items ->
            div [] (List.map viewItem items)

        _ ->
            div [] [ text "no data yet" ]


viewItem : Item -> Html Msg
viewItem item =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "card border" ]
                [ viewCardImage item.image
                , div [ class ("card-body alert-" ++ dangerLevelClass item.dueInDays) ]
                    [ div [ class "" ]
                        [ div [ class ("float-right days-left-badge badge badge-" ++ dangerLevelClass item.dueInDays) ]
                            [ text (daysLeft item.dueInDays)

                            {--, br [] [], text item.status --}
                            ]

                        -- , div [ class ("float-right badge badge-" ++ dangerLevelClass item.dueInDays) ] [ text item.status ]
                        ]
                    , h5 [ class "card-title" ] [ text item.title ]
                    , h6 [ class "card-subtitle" ] [ text item.who ]
                    , div [ class "" ]
                        (List.concat
                            [ Maybe.withDefault [] (Maybe.map (\s -> [ div [ class "" ] [ text s ] ]) item.renewed)
                            , [ div [ class "" ] [ text item.status ] ]
                            ]
                        )
                    ]
                ]
            ]
        ]


daysLeft days =
    case days of
        1 ->
            "1 day left"

        _ ->
            String.fromInt days ++ " days left"


dangerLevelClass : Int -> String
dangerLevelClass days =
    if days <= 2 then
        "danger"

    else if days <= 5 then
        "warning"

    else
        "success"


viewCardImage : Maybe ItemImage -> Html Msg
viewCardImage image =
    case image of
        Nothing ->
            text ""

        Just url ->
            img [ src url.normal, class "card-img-top" ] []


viewSettingsData : WebData Settings -> Html Msg
viewSettingsData settingsData =
    case settingsData of
        RemoteData.Success settings ->
            viewSettings settings

        RemoteData.Failure err ->
            div [ class "alert alert-danger" ] [ text (Debug.toString err) ]

        RemoteData.NotAsked ->
            text "not yet loaded"

        RemoteData.Loading ->
            text "Loading..."


viewSettings : Settings -> Html Msg
viewSettings settings =
    let
        fieldsets i cred =
            fieldset []
                [ div [ class "form-group" ]
                    [ label [] [ text "Name" ]
                    , input [ value cred.name, onInput (SetCredentialField i Name), type_ "text", class "form-control" ] []
                    ]
                ]
    in
    div []
        (List.concat
            [ [ button [ class "btn btn-primary", onClick AppendCredentials ] [ text "Add Credentials" ] ]
            , List.indexedMap fieldsets settings.credentials
            ]
        )


main : Program Settings Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


encodeSettings : Settings -> JE.Value
encodeSettings settings =
    JE.object
        [ ( "credentials", JE.list encodeCredential settings.credentials ) ]


encodeCredential : Credential -> JE.Value
encodeCredential cred =
    JE.object
        [ ( "rootUrl", JE.string cred.rootUrl )
        , ( "tz", JE.string cred.tz )
        , ( "name", JE.string cred.name )
        , ( "barcode", JE.string cred.barcode )
        , ( "pin", JE.string cred.pin )
        ]


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
