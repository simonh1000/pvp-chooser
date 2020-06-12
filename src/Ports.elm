port module Ports exposing (..)

import Json.Encode exposing (Value)


port toJs : PortMsg -> Cmd msg


type alias PortMsg =
    { tag : String
    , payload : Value
    }


persist : Value -> Cmd msg
persist payload =
    toJs { tag = "Persist", payload = payload }
