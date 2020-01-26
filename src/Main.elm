module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, contenteditable, id)
import Html.Events exposing (custom, on)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent)



---- MODEL ----


type Variant
    = Text


type alias Block =
    { id : Int
    , text : String
    , variant : Variant
    }


type alias Content =
    List Block


type alias Model =
    { content : Content }


init : ( Model, Cmd Msg )
init =
    ( { content =
            [ { id = 0
              , text = "Hello, world"
              , variant = Text
              }
            ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = CreateEmptyBlock
    | Edit Block
    | Delete Block
    | OnEnter
    | Noop


addBlock : Content -> Block -> Content
addBlock content block =
    List.concat [ content, [ block ] ]


deleteBlock : Content -> Block -> Content
deleteBlock content block =
    List.filter (\b -> b.id == block.id) content


editBlock : Content -> Block -> Content
editBlock content block =
    List.map
        (\b ->
            if b.id == block.id then
                block

            else
                b
        )
        content


newBlock : Content -> Block
newBlock content =
    { id =
        case List.maximum (List.map (\block -> block.id) content) of
            Just max ->
                max + 1

            Nothing ->
                0
    , text = ""
    , variant = Text
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateEmptyBlock ->
            ( { model | content = addBlock model.content (newBlock model.content) }, Cmd.none )

        Delete block ->
            ( { model | content = deleteBlock model.content block }, Cmd.none )

        Edit block ->
            ( { model | content = editBlock model.content block }, Cmd.none )

        OnEnter ->
            update CreateEmptyBlock model

        Noop ->
            ( model, Cmd.none )



---- VIEW ----


keyDecoder : Json.Decoder ( Msg, Bool )
keyDecoder =
    Json.field "key" Json.string
        |> Json.map
            (\key ->
                ( if key == "Enter" then
                    CreateEmptyBlock

                  else
                    Noop
                , key == "Enter"
                )
            )


renderBlock : Block -> Html Msg
renderBlock block =
    div
        [ class "block"
        , contenteditable True
        , Html.Events.preventDefaultOn "keydown" keyDecoder
        ]
        [ text block.text
        ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Notes" ]
        , div
            [ id "editor"
            , class "editor"
            ]
            (List.map
                renderBlock
                model.content
            )
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
