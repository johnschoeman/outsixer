module Player exposing (Player, Role, assignRoles)

import List.Extra exposing (getAt, removeAt)
import Random exposing (Seed, initialSeed, int, step)


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


assignRoles : Int -> List Player -> List Player
assignRoles seedInt players =
    let
        shuffled =
            shuffleList (initialSeed seedInt) players
    in
    List.indexedMap
        (\idx player ->
            case idx of
                0 ->
                    { player | role = Just "Master" }

                1 ->
                    { player | role = Just "Insider" }

                _ ->
                    { player | role = Just "Commoner" }
        )
        shuffled


shuffleList : Seed -> List a -> List a
shuffleList seed list =
    shuffleListHelper seed list []


shuffleListHelper : Seed -> List a -> List a -> List a
shuffleListHelper seed source result =
    if List.isEmpty source then
        result

    else
        let
            indexGenerator =
                int 0 (List.length source - 1)

            ( index, nextSeed ) =
                step indexGenerator seed

            valAtIndex =
                getAt index source

            sourceWithoutIndex =
                removeAt index source
        in
        case valAtIndex of
            Just val ->
                shuffleListHelper nextSeed sourceWithoutIndex (val :: result)

            Nothing ->
                source
