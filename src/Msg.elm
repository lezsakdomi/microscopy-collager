module Msg exposing (..)

import Downloading
import File exposing (File)
import Interface.Msg as Interface


type Msg
    = InterfaceMsg Interface.Msg
    | DownloadingMsg Downloading.Msg
    | GotImgSize Int ( Int, Int )
