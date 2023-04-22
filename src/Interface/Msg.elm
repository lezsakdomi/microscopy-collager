module Interface.Msg exposing (..)

import File exposing (File)
import Model exposing (Direction, Image)


type Msg
    = GotImageFiles (List File)
    | GotImages (List Image)
    | GotImageScalingPx Int String
    | GotImageScalingUm Int String
    | GotCroppedSize String
    | GotRealSize String
    | GotGap String
    | MoveImage Int (Maybe Int)
    | RemoveBackground
    | GotBackground { color : String, opacity : Float }
    | GotDirection String
    | GotWrapAt String
    | GotFocus (Maybe Int)
