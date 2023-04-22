module Rendering exposing (Attribute(..), renderHtml, renderString)

import Html as Basic
import Model exposing (Direction(..), Image, StaticModel)
import Svg.String as Svg exposing (..)
import Svg.String.Attributes as Attributes exposing (..)


type Attribute
    = Style String String
    | Attribute String String


renderHtml : StaticModel -> Maybe Int -> List Attribute -> Basic.Html Never
renderHtml model focusedImage attributes =
    Svg.toHtml <| render model focusedImage attributes


renderString : Int -> StaticModel -> List Attribute -> String
renderString indent model attributes =
    Svg.toString indent <| render model Nothing attributes


render : StaticModel -> Maybe Int -> List Attribute -> Svg.Html Never
render model focusedImage attributes =
    let
        positionShiftFor index =
            let
                i =
                    index // model.wrapAt

                j =
                    remainderBy model.wrapAt index

                ( row, column ) =
                    case model.direction of
                        Column ->
                            ( i, j )

                        Row ->
                            ( j, i )
            in
            ( toFloat model.gap + toFloat column * toFloat (model.gap + model.realSize), toFloat model.gap + toFloat row * toFloat (model.gap + model.realSize) )

        renderImage : Int -> Image -> Svg Never
        renderImage i image =
            let
                positionShift =
                    positionShiftFor i

                visibleSize =
                    model.croppedSize * toFloat image.scaling.px / image.scaling.um
            in
            node "svg"
                [ x <| String.fromFloat <| Tuple.first positionShift
                , y <| String.fromFloat <| Tuple.second positionShift
                , width <| String.fromInt model.realSize
                , height <| String.fromInt model.realSize
                , viewBox <|
                    String.join " "
                        [ String.fromFloat <| toFloat (Tuple.first image.center) - visibleSize / 2
                        , String.fromFloat <| toFloat (Tuple.second image.center) - visibleSize / 2
                        , String.fromFloat visibleSize
                        , String.fromFloat visibleSize
                        ]
                ]
                [ node "image" [ attribute "href" image.url ] [] ]

        viewSize : ( Int, Int )
        viewSize =
            let
                n1 =
                    ceiling <| toFloat (List.length model.images) / toFloat model.wrapAt

                n2 =
                    Basics.min (List.length model.images) model.wrapAt

                ( rows, columns ) =
                    case model.direction of
                        Column ->
                            ( n2, n1 )

                        Row ->
                            ( n1, n2 )
            in
            ( rows * (model.gap + model.realSize) + model.gap, columns * (model.gap + model.realSize) + model.gap )

        ( w, h ) =
            Tuple.mapBoth String.fromInt String.fromInt viewSize
    in
    svg
        ([ viewBox <| "0 0 " ++ w ++ " " ++ h, width w, height h ]
            ++ (List.filterMap
                    (\attrDef ->
                        case attrDef of
                            Style k v ->
                                Nothing

                            Attribute k v ->
                                Just <| attribute k v
                    )
                    attributes
                    ++ (case
                            List.filterMap
                                (\attrDef ->
                                    case attrDef of
                                        Style k v ->
                                            Just <| k ++ ": " ++ v ++ ";"

                                        Attribute k v ->
                                            Nothing
                                )
                                attributes
                        of
                            [] ->
                                []

                            l ->
                                [ style <| String.join " " l ]
                       )
               )
        )
    <|
        []
            ++ (model.background |> Maybe.map (\{ color, opacity } -> [ rect [ x "0", y "0", width w, height h, fill color, fillOpacity <| String.fromFloat opacity ] [] ]) |> Maybe.withDefault [])
            ++ List.indexedMap renderImage model.images
            ++ (focusedImage
                    |> Maybe.map
                        (\i ->
                            let
                                ( xx, yy ) =
                                    Tuple.mapBoth String.fromFloat String.fromFloat <| positionShiftFor i

                                ( ww, hh ) =
                                    Tuple.mapBoth String.fromInt String.fromInt <| ( model.realSize, model.realSize )
                            in
                            [ rect [ x xx, y yy, width ww, height hh, stroke "grey", fill "white", fillOpacity "0.5" ] []
                            ]
                        )
                    |> Maybe.withDefault []
               )
