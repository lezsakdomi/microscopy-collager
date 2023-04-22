module Images exposing (handler, loadHandler)

import File exposing (File)
import Interface.Msg as Interface exposing (Msg(..))
import Json.Decode as D
import Msg as Global exposing (Msg(..))


handler : D.Decoder Interface.Msg
handler =
    D.map GotImageFiles filesDecoder


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)


loadHandler : Int -> D.Decoder Global.Msg
loadHandler i =
    decodeImgSize <| GotImgSize i


decodeImgSize : (( Int, Int ) -> msg) -> D.Decoder msg
decodeImgSize msg =
    D.map msg <|
        D.field "target" <|
            D.map2 Tuple.pair
                (D.field "width" D.int)
                (D.field "height" D.int)
