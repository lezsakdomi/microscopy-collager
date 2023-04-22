module Interface.View exposing (view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html as Html_
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events exposing (..)
import Html.Styled.Keyed as Keyed
import Images
import Interface.Msg exposing (Msg(..))
import Json.Decode as D
import Maybe.Extra as Maybe
import Model exposing (..)


onClickPreventDefault msg =
    preventDefaultOn "click" <| D.succeed ( msg, True )


imageButtonStyle : Style
imageButtonStyle =
    Css.batch
        [ padding zero
        , border zero
        , cursor pointer
        ]


viewImage : Int -> Image -> ( String, Html.Html Msg )
viewImage i image =
    ( "image_" ++ image.name
    , div
        [ css
            [ padding <| px 5
            , displayFlex
            , flexDirection row
            , Css.property "gap" "5px"
            , hover
                [ backgroundColor <| rgb 221 221 221
                ]
            , transition
                [ Transitions.background 200
                ]
            ]
        , onMouseEnter <| GotFocus <| Just i
        , onMouseLeave <| GotFocus Nothing
        ]
        [ b [] [ text <| String.fromChar <| Char.fromCode <| Char.toCode 'A' + i ]
        , div
            [ css
                [ flexGrow <| int 1
                ]
            ]
            [ text image.name ]
        , span []
            [ input
                [ type_ "number"
                , value <| String.fromInt image.scaling.px
                , onInput <| GotImageScalingPx i
                , css [ Css.width <| Css.em 4 ]
                , step "1"
                ]
                []
            , text "px = "
            , input
                [ type_ "number"
                , value <| String.fromFloat image.scaling.um
                , onInput <| GotImageScalingUm i
                , css [ Css.width <| Css.em 4 ]
                , step "0.1"
                ]
                []
            , text "µm"
            ]
        , button [ css [ imageButtonStyle ], type_ "button", onClickPreventDefault <| MoveImage i <| Just -1 ] [ text "🡅" ]
        , button [ css [ imageButtonStyle ], type_ "button", onClickPreventDefault <| MoveImage i <| Just 1 ] [ text "🡇" ]
        , button [ css [ imageButtonStyle ], type_ "button", onClickPreventDefault <| MoveImage i <| Nothing ] [ text "🗑️" ]
        ]
    )


view : Model -> Html_.Html Msg
view { model } =
    toUnstyled <|
        Html.form
            [ css
                [ displayFlex
                , flexDirection column
                ]
            ]
            [ div []
                [ text "Layout: "
                , input
                    [ type_ "number"
                    , value <| String.fromInt model.wrapAt
                    , onInput GotWrapAt
                    , css [ Css.width <| Css.em 2 ]
                    ]
                    []
                , text " images in a "
                , select [ onInput GotDirection ]
                    [ option [ selected <| model.direction == Column ] [ text "column" ]
                    , option [ selected <| model.direction == Row ] [ text "row" ]
                    ]
                ]
            , div []
                [ text "Size: "
                , input
                    [ type_ "number"
                    , value <| String.fromFloat model.croppedSize
                    , css [ Css.width <| Css.em 3 ]
                    , onInput GotCroppedSize
                    , step "any"
                    ]
                    []
                , text "µm scaled to "
                , input
                    [ type_ "number"
                    , value <| String.fromInt model.realSize
                    , css [ Css.width <| Css.em 3 ]
                    , onInput GotRealSize
                    ]
                    []
                , text "px squares with "
                , input
                    [ type_ "number"
                    , value <| String.fromInt model.gap
                    , css [ Css.width <| Css.em 2 ]
                    , onInput GotGap
                    ]
                    []
                , text "px gaps"
                ]
            , div [] <|
                case model.background of
                    Nothing ->
                        [ input
                            [ type_ "checkbox"
                            , id "bgCheckbox"
                            , Attributes.checked <| Maybe.isJust model.background
                            , onClick <| GotBackground { color = "transparent", opacity = 1 }
                            ]
                            []
                        , label [ for "bgCheckbox" ] [ text "Background" ]
                        ]

                    Just { color, opacity } ->
                        [ input
                            [ type_ "checkbox"
                            , id "bgCheckbox"
                            , Attributes.checked <| Maybe.isJust model.background
                            , onClick RemoveBackground
                            ]
                            []
                        , label [ for "bgCheckbox" ] [ text "Background: " ]
                        , input
                            [ type_ "text"
                            , value color
                            , onInput (\str -> GotBackground { color = str, opacity = opacity })
                            , css [ Css.width <| Css.em 5 ]
                            ]
                            []
                        ]
            , Keyed.node "div"
                [ css
                    [ displayFlex
                    , flexDirection column
                    , padding <| Css.em 0.5
                    , margin <| Css.em 0.5
                    , border2 (px 1) solid
                    ]
                ]
              <|
                List.indexedMap viewImage model.images
                    ++ [ ( "_add"
                         , input
                            [ type_ "file"
                            , accept "image/png,image/jpeg,image/svg"
                            , multiple True
                            , on "change" Images.handler
                            , value ""
                            , css
                                [ backgroundColor <| rgb 211 211 211
                                , cursor pointer
                                ]
                            ]
                            []
                         )
                       ]
            ]
