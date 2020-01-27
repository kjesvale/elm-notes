module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, contenteditable, id)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Json
import Task



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


getNextId : Content -> Int
getNextId content =
    case List.maximum (List.map (\block -> block.id) content) of
        Just max ->
            max + 1

        Nothing ->
            0


generateEmptyBlock : Int -> Block
generateEmptyBlock id =
    { id = id
    , text = ""
    , variant = Text
    }


focusBlock : Block -> Cmd Msg
focusBlock block =
    Task.attempt (always Noop) (Dom.focus ("block-" ++ String.fromInt block.id))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateEmptyBlock ->
            let
                emptyBlock =
                    generateEmptyBlock (getNextId model.content)
            in
            ( { model | content = addBlock model.content emptyBlock }, focusBlock emptyBlock )

        Delete block ->
            ( { model | content = deleteBlock model.content block }, Cmd.none )

        Edit block ->
            ( { model | content = editBlock model.content block }, Cmd.none )

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
        [ id ("block-" ++ String.fromInt block.id)
        , class "block"
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
