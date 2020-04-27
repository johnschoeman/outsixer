module Game exposing (Game)

import Player exposing (Player)


type alias Game =
    { id : Int
    , name : String
    , word : String
    , active : Bool
    , players : List Player
    }
