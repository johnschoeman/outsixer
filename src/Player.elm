module Player exposing (Player, Role)


type alias Player =
    { id : Int
    , name : String
    , gameId : Int
    , role : Maybe String
    }


type Role
    = Master
    | Insider
    | Commoner
