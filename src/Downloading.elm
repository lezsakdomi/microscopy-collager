module Downloading exposing (Msg, update, view)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Model exposing (Model, RenderingModel)
import Rendering exposing (Attribute(..), renderString)
import Url


type Msg
    = Render


view : Model -> Html Msg
view { rendering } =
    H.p [] <|
        case rendering.downloadUrl of
            Nothing ->
                [ H.button
                    [ A.type_ "button"
                    , E.onClick Render
                    ]
                    [ H.text "Render image" ]
                ]

            Just downloadUrl ->
                [ H.a
                    [ A.href downloadUrl
                    , A.download "image.svg"
                    ]
                    [ H.text "Download image" ]
                ]



--Html.pre [ A.style "overflow" "auto" ] [ Html.Lazy.lazy (Html.text << renderString 0) { model | focusedImage = Nothing } ]


update : Msg -> Model -> RenderingModel
update msg { model, rendering } =
    case msg of
        Render ->
            { rendering
                | downloadUrl =
                    Just <|
                        "data:image/svg+xml,<?xml version=\"1.0\"?>"
                            ++ Url.percentEncode (renderString 0 model [ Attribute "xmlns" "http://www.w3.org/2000/svg" ])
            }
