module Interface.Update exposing (..)

import File
import Interface.Msg exposing (..)
import List.Extra as List
import Model exposing (Direction(..), Model, newRendering)
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { client, model, rendering } =
    let
        noop =
            ( Model client model rendering, Cmd.none )
    in
    case msg of
        GotImageFiles files ->
            ( Model client model newRendering, Task.perform GotImages <| Task.sequence <| List.map (\file -> Task.map (Model.emptyImage <| File.name file) <| File.toUrl file) files )

        GotImages images ->
            ( Model client { model | images = model.images ++ images } newRendering, Cmd.none )

        GotCroppedSize string ->
            case String.toFloat string of
                Just parsedValue ->
                    ( Model client { model | croppedSize = parsedValue } newRendering, Cmd.none )

                Nothing ->
                    noop

        GotRealSize string ->
            case String.toInt string of
                Just parsedValue ->
                    ( Model client { model | realSize = parsedValue } newRendering, Cmd.none )

                Nothing ->
                    noop

        GotGap string ->
            case String.toInt string of
                Just parsedValue ->
                    ( Model client { model | gap = parsedValue } newRendering, Cmd.none )

                Nothing ->
                    noop

        GotImageScalingPx int string ->
            case String.toInt string of
                Just parsedValue ->
                    ( Model client
                        { model
                            | images =
                                List.updateAt int
                                    (\image ->
                                        let
                                            imageScaling =
                                                image.scaling
                                        in
                                        { image | scaling = { imageScaling | px = parsedValue } }
                                    )
                                    model.images
                        }
                        newRendering
                    , Cmd.none
                    )

                Nothing ->
                    noop

        GotImageScalingUm int string ->
            case String.toFloat string of
                Just parsedValue ->
                    ( Model client
                        { model
                            | images =
                                List.updateAt int
                                    (\image ->
                                        let
                                            imageScaling =
                                                image.scaling
                                        in
                                        { image | scaling = { imageScaling | um = parsedValue } }
                                    )
                                    model.images
                        }
                        newRendering
                    , Cmd.none
                    )

                Nothing ->
                    noop

        MoveImage int maybeDelta ->
            case maybeDelta of
                Just delta ->
                    case List.getAt int model.images of
                        Just image ->
                            let
                                ( before, after ) =
                                    List.splitAt (int + delta) (List.removeAt int model.images)
                            in
                            ( Model client { model | images = before ++ [ image ] ++ after } newRendering, Cmd.none )

                        Nothing ->
                            ( Model client model rendering, Cmd.none )

                Nothing ->
                    ( Model client { model | images = List.removeAt int model.images } newRendering, Cmd.none )

        RemoveBackground ->
            ( Model client { model | background = Nothing } newRendering, Cmd.none )

        GotBackground string ->
            ( Model client { model | background = Just string } newRendering, Cmd.none )

        GotDirection str ->
            case str of
                "row" ->
                    ( Model client { model | direction = Row } newRendering, Cmd.none )

                "column" ->
                    ( Model client { model | direction = Column } newRendering, Cmd.none )

                _ ->
                    noop

        GotWrapAt string ->
            case String.toInt string of
                Just parsedValue ->
                    ( Model client { model | wrapAt = parsedValue } newRendering, Cmd.none )

                Nothing ->
                    noop

        GotFocus maybeInt ->
            ( Model { client | focusedImage = maybeInt } model rendering, Cmd.none )
