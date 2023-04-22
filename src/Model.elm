module Model exposing (..)


type alias Pixels =
    Int


type alias Micrometers =
    Float


type alias Image =
    { scaling : { px : Pixels, um : Micrometers }
    , size : ( Pixels, Pixels )
    , center : ( Pixels, Pixels )
    , name : String
    , url : String
    }


emptyImage name url =
    Image { px = 100, um = 50 } ( 0, 0 ) ( 0, 0 ) name url


type Direction
    = Column
    | Row


type alias ViewModel =
    { focusedImage : Maybe Int
    }


newView : ViewModel
newView =
    ViewModel Nothing


type alias RenderingModel =
    { downloadUrl : Maybe String
    }


newRendering : RenderingModel
newRendering =
    RenderingModel Nothing


type alias StaticModel =
    { images : List Image
    , wrapAt : Int
    , direction : Direction
    , croppedSize : Micrometers
    , realSize : Pixels
    , gap : Pixels
    , background : Maybe { color : String, opacity : Float }
    }


newModel : StaticModel
newModel =
    { images = []
    , wrapAt = 2
    , direction = Row
    , croppedSize = 50
    , realSize = 50
    , gap = 5
    , background = Just { color = "grey", opacity = 0.5 }
    }


type alias Model =
    { client : ViewModel
    , model : StaticModel
    , rendering : RenderingModel
    }


empty : Model
empty =
    Model newView newModel newRendering
