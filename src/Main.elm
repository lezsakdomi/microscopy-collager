module Main exposing (main)

import Browser exposing (document)
import Downloading
import Html
import Html.Attributes as A
import Html.Events
import Images
import Interface.Update as Interface
import Interface.View as Interface
import List.Extra as List
import Model exposing (Model)
import Msg exposing (Msg(..))
import Rendering exposing (renderHtml)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd msg )
init _ =
    ( Model.empty, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        staticModel =
            model.model
    in
    { title = "Microscopy collage maker"
    , body =
        [ Html.map InterfaceMsg <| Interface.view model

        -- , Html.pre [ A.style "overflow" "auto" ] [ Html.text <| toString { staticModel | images = [] } ]
        , Html.map never <|
            renderHtml model.model
                model.client.focusedImage
                [ Rendering.Style "margin" "1em"
                , Rendering.Style "border" "1px dashed black"
                , Rendering.Attribute "id" "image"
                ]
        , Html.div [ A.style "display" "none" ] <|
            List.indexedMap
                (\i image ->
                    Html.img
                        [ A.src image.url, Html.Events.on "load" <| Images.loadHandler i ]
                        []
                )
                model.model.images
        , Html.map DownloadingMsg <| Downloading.view model
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InterfaceMsg interfaceMsg ->
            Tuple.mapSecond (Cmd.map InterfaceMsg) <|
                Interface.update interfaceMsg model

        GotImgSize i size ->
            let
                staticModel =
                    model.model

                ( width, height ) =
                    size
            in
            ( { model
                | model =
                    { staticModel
                        | images =
                            List.updateAt i
                                (\image ->
                                    { image
                                        | size = size
                                        , center = ( round <| toFloat width / 2, round <| toFloat height / 2 )
                                    }
                                )
                                staticModel.images
                    }
              }
            , Cmd.none
            )

        DownloadingMsg downloadingMsg ->
            ( { model | rendering = Downloading.update downloadingMsg model }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
