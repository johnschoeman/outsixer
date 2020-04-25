-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.InputObject exposing (..)

import Api.Enum.Games_constraint
import Api.Enum.Games_update_column
import Api.Enum.Order_by
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


buildBoolean_comparison_exp : (Boolean_comparison_expOptionalFields -> Boolean_comparison_expOptionalFields) -> Boolean_comparison_exp
buildBoolean_comparison_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { eq_ = Absent, gt_ = Absent, gte_ = Absent, in_ = Absent, is_null_ = Absent, lt_ = Absent, lte_ = Absent, neq_ = Absent, nin_ = Absent }
    in
    { eq_ = optionals.eq_, gt_ = optionals.gt_, gte_ = optionals.gte_, in_ = optionals.in_, is_null_ = optionals.is_null_, lt_ = optionals.lt_, lte_ = optionals.lte_, neq_ = optionals.neq_, nin_ = optionals.nin_ }


type alias Boolean_comparison_expOptionalFields =
    { eq_ : OptionalArgument Bool
    , gt_ : OptionalArgument Bool
    , gte_ : OptionalArgument Bool
    , in_ : OptionalArgument (List Bool)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument Bool
    , lte_ : OptionalArgument Bool
    , neq_ : OptionalArgument Bool
    , nin_ : OptionalArgument (List Bool)
    }


{-| Type for the Boolean\_comparison\_exp input object.
-}
type alias Boolean_comparison_exp =
    { eq_ : OptionalArgument Bool
    , gt_ : OptionalArgument Bool
    , gte_ : OptionalArgument Bool
    , in_ : OptionalArgument (List Bool)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument Bool
    , lte_ : OptionalArgument Bool
    , neq_ : OptionalArgument Bool
    , nin_ : OptionalArgument (List Bool)
    }


{-| Encode a Boolean\_comparison\_exp into a value that can be used as an argument.
-}
encodeBoolean_comparison_exp : Boolean_comparison_exp -> Value
encodeBoolean_comparison_exp input =
    Encode.maybeObject
        [ ( "_eq", Encode.bool |> Encode.optional input.eq_ ), ( "_gt", Encode.bool |> Encode.optional input.gt_ ), ( "_gte", Encode.bool |> Encode.optional input.gte_ ), ( "_in", (Encode.bool |> Encode.list) |> Encode.optional input.in_ ), ( "_is_null", Encode.bool |> Encode.optional input.is_null_ ), ( "_lt", Encode.bool |> Encode.optional input.lt_ ), ( "_lte", Encode.bool |> Encode.optional input.lte_ ), ( "_neq", Encode.bool |> Encode.optional input.neq_ ), ( "_nin", (Encode.bool |> Encode.list) |> Encode.optional input.nin_ ) ]


buildGames_aggregate_order_by : (Games_aggregate_order_byOptionalFields -> Games_aggregate_order_byOptionalFields) -> Games_aggregate_order_by
buildGames_aggregate_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { avg = Absent, count = Absent, max = Absent, min = Absent, stddev = Absent, stddev_pop = Absent, stddev_samp = Absent, sum = Absent, var_pop = Absent, var_samp = Absent, variance = Absent }
    in
    { avg = optionals.avg, count = optionals.count, max = optionals.max, min = optionals.min, stddev = optionals.stddev, stddev_pop = optionals.stddev_pop, stddev_samp = optionals.stddev_samp, sum = optionals.sum, var_pop = optionals.var_pop, var_samp = optionals.var_samp, variance = optionals.variance }


type alias Games_aggregate_order_byOptionalFields =
    { avg : OptionalArgument Games_avg_order_by
    , count : OptionalArgument Api.Enum.Order_by.Order_by
    , max : OptionalArgument Games_max_order_by
    , min : OptionalArgument Games_min_order_by
    , stddev : OptionalArgument Games_stddev_order_by
    , stddev_pop : OptionalArgument Games_stddev_pop_order_by
    , stddev_samp : OptionalArgument Games_stddev_samp_order_by
    , sum : OptionalArgument Games_sum_order_by
    , var_pop : OptionalArgument Games_var_pop_order_by
    , var_samp : OptionalArgument Games_var_samp_order_by
    , variance : OptionalArgument Games_variance_order_by
    }


{-| Type for the Games\_aggregate\_order\_by input object.
-}
type alias Games_aggregate_order_by =
    { avg : OptionalArgument Games_avg_order_by
    , count : OptionalArgument Api.Enum.Order_by.Order_by
    , max : OptionalArgument Games_max_order_by
    , min : OptionalArgument Games_min_order_by
    , stddev : OptionalArgument Games_stddev_order_by
    , stddev_pop : OptionalArgument Games_stddev_pop_order_by
    , stddev_samp : OptionalArgument Games_stddev_samp_order_by
    , sum : OptionalArgument Games_sum_order_by
    , var_pop : OptionalArgument Games_var_pop_order_by
    , var_samp : OptionalArgument Games_var_samp_order_by
    , variance : OptionalArgument Games_variance_order_by
    }


{-| Encode a Games\_aggregate\_order\_by into a value that can be used as an argument.
-}
encodeGames_aggregate_order_by : Games_aggregate_order_by -> Value
encodeGames_aggregate_order_by input =
    Encode.maybeObject
        [ ( "avg", encodeGames_avg_order_by |> Encode.optional input.avg ), ( "count", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.count ), ( "max", encodeGames_max_order_by |> Encode.optional input.max ), ( "min", encodeGames_min_order_by |> Encode.optional input.min ), ( "stddev", encodeGames_stddev_order_by |> Encode.optional input.stddev ), ( "stddev_pop", encodeGames_stddev_pop_order_by |> Encode.optional input.stddev_pop ), ( "stddev_samp", encodeGames_stddev_samp_order_by |> Encode.optional input.stddev_samp ), ( "sum", encodeGames_sum_order_by |> Encode.optional input.sum ), ( "var_pop", encodeGames_var_pop_order_by |> Encode.optional input.var_pop ), ( "var_samp", encodeGames_var_samp_order_by |> Encode.optional input.var_samp ), ( "variance", encodeGames_variance_order_by |> Encode.optional input.variance ) ]


buildGames_arr_rel_insert_input : Games_arr_rel_insert_inputRequiredFields -> (Games_arr_rel_insert_inputOptionalFields -> Games_arr_rel_insert_inputOptionalFields) -> Games_arr_rel_insert_input
buildGames_arr_rel_insert_input required fillOptionals =
    let
        optionals =
            fillOptionals
                { on_conflict = Absent }
    in
    Games_arr_rel_insert_input { data = required.data, on_conflict = optionals.on_conflict }


type alias Games_arr_rel_insert_inputRequiredFields =
    { data : List Games_insert_input }


type alias Games_arr_rel_insert_inputOptionalFields =
    { on_conflict : OptionalArgument Games_on_conflict }


{-| Type alias for the `Games_arr_rel_insert_input` attributes. Note that this type
needs to use the `Games_arr_rel_insert_input` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias Games_arr_rel_insert_inputRaw =
    { data : List Games_insert_input
    , on_conflict : OptionalArgument Games_on_conflict
    }


{-| Type for the Games\_arr\_rel\_insert\_input input object.
-}
type Games_arr_rel_insert_input
    = Games_arr_rel_insert_input Games_arr_rel_insert_inputRaw


{-| Encode a Games\_arr\_rel\_insert\_input into a value that can be used as an argument.
-}
encodeGames_arr_rel_insert_input : Games_arr_rel_insert_input -> Value
encodeGames_arr_rel_insert_input (Games_arr_rel_insert_input input) =
    Encode.maybeObject
        [ ( "data", (encodeGames_insert_input |> Encode.list) input.data |> Just ), ( "on_conflict", encodeGames_on_conflict |> Encode.optional input.on_conflict ) ]


buildGames_avg_order_by : (Games_avg_order_byOptionalFields -> Games_avg_order_byOptionalFields) -> Games_avg_order_by
buildGames_avg_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_avg_order_byOptionalFields =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Type for the Games\_avg\_order\_by input object.
-}
type alias Games_avg_order_by =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Encode a Games\_avg\_order\_by into a value that can be used as an argument.
-}
encodeGames_avg_order_by : Games_avg_order_by -> Value
encodeGames_avg_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ) ]


buildGames_bool_exp : (Games_bool_expOptionalFields -> Games_bool_expOptionalFields) -> Games_bool_exp
buildGames_bool_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { and_ = Absent, not_ = Absent, or_ = Absent, active = Absent, created_at = Absent, id = Absent, name = Absent, updated_at = Absent }
    in
    Games_bool_exp { and_ = optionals.and_, not_ = optionals.not_, or_ = optionals.or_, active = optionals.active, created_at = optionals.created_at, id = optionals.id, name = optionals.name, updated_at = optionals.updated_at }


type alias Games_bool_expOptionalFields =
    { and_ : OptionalArgument (List (Maybe Games_bool_exp))
    , not_ : OptionalArgument Games_bool_exp
    , or_ : OptionalArgument (List (Maybe Games_bool_exp))
    , active : OptionalArgument Boolean_comparison_exp
    , created_at : OptionalArgument Timestamptz_comparison_exp
    , id : OptionalArgument Int_comparison_exp
    , name : OptionalArgument String_comparison_exp
    , updated_at : OptionalArgument Timestamptz_comparison_exp
    }


{-| Type alias for the `Games_bool_exp` attributes. Note that this type
needs to use the `Games_bool_exp` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias Games_bool_expRaw =
    { and_ : OptionalArgument (List (Maybe Games_bool_exp))
    , not_ : OptionalArgument Games_bool_exp
    , or_ : OptionalArgument (List (Maybe Games_bool_exp))
    , active : OptionalArgument Boolean_comparison_exp
    , created_at : OptionalArgument Timestamptz_comparison_exp
    , id : OptionalArgument Int_comparison_exp
    , name : OptionalArgument String_comparison_exp
    , updated_at : OptionalArgument Timestamptz_comparison_exp
    }


{-| Type for the Games\_bool\_exp input object.
-}
type Games_bool_exp
    = Games_bool_exp Games_bool_expRaw


{-| Encode a Games\_bool\_exp into a value that can be used as an argument.
-}
encodeGames_bool_exp : Games_bool_exp -> Value
encodeGames_bool_exp (Games_bool_exp input) =
    Encode.maybeObject
        [ ( "_and", (encodeGames_bool_exp |> Encode.maybe |> Encode.list) |> Encode.optional input.and_ ), ( "_not", encodeGames_bool_exp |> Encode.optional input.not_ ), ( "_or", (encodeGames_bool_exp |> Encode.maybe |> Encode.list) |> Encode.optional input.or_ ), ( "active", encodeBoolean_comparison_exp |> Encode.optional input.active ), ( "created_at", encodeTimestamptz_comparison_exp |> Encode.optional input.created_at ), ( "id", encodeInt_comparison_exp |> Encode.optional input.id ), ( "name", encodeString_comparison_exp |> Encode.optional input.name ), ( "updated_at", encodeTimestamptz_comparison_exp |> Encode.optional input.updated_at ) ]


buildGames_inc_input : (Games_inc_inputOptionalFields -> Games_inc_inputOptionalFields) -> Games_inc_input
buildGames_inc_input fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_inc_inputOptionalFields =
    { id : OptionalArgument Int }


{-| Type for the Games\_inc\_input input object.
-}
type alias Games_inc_input =
    { id : OptionalArgument Int }


{-| Encode a Games\_inc\_input into a value that can be used as an argument.
-}
encodeGames_inc_input : Games_inc_input -> Value
encodeGames_inc_input input =
    Encode.maybeObject
        [ ( "id", Encode.int |> Encode.optional input.id ) ]


buildGames_insert_input : (Games_insert_inputOptionalFields -> Games_insert_inputOptionalFields) -> Games_insert_input
buildGames_insert_input fillOptionals =
    let
        optionals =
            fillOptionals
                { active = Absent, created_at = Absent, id = Absent, name = Absent, updated_at = Absent }
    in
    { active = optionals.active, created_at = optionals.created_at, id = optionals.id, name = optionals.name, updated_at = optionals.updated_at }


type alias Games_insert_inputOptionalFields =
    { active : OptionalArgument Bool
    , created_at : OptionalArgument Api.ScalarCodecs.Timestamptz
    , id : OptionalArgument Int
    , name : OptionalArgument String
    , updated_at : OptionalArgument Api.ScalarCodecs.Timestamptz
    }


{-| Type for the Games\_insert\_input input object.
-}
type alias Games_insert_input =
    { active : OptionalArgument Bool
    , created_at : OptionalArgument Api.ScalarCodecs.Timestamptz
    , id : OptionalArgument Int
    , name : OptionalArgument String
    , updated_at : OptionalArgument Api.ScalarCodecs.Timestamptz
    }


{-| Encode a Games\_insert\_input into a value that can be used as an argument.
-}
encodeGames_insert_input : Games_insert_input -> Value
encodeGames_insert_input input =
    Encode.maybeObject
        [ ( "active", Encode.bool |> Encode.optional input.active ), ( "created_at", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.created_at ), ( "id", Encode.int |> Encode.optional input.id ), ( "name", Encode.string |> Encode.optional input.name ), ( "updated_at", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.updated_at ) ]


buildGames_max_order_by : (Games_max_order_byOptionalFields -> Games_max_order_byOptionalFields) -> Games_max_order_by
buildGames_max_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { created_at = Absent, id = Absent, name = Absent, updated_at = Absent }
    in
    { created_at = optionals.created_at, id = optionals.id, name = optionals.name, updated_at = optionals.updated_at }


type alias Games_max_order_byOptionalFields =
    { created_at : OptionalArgument Api.Enum.Order_by.Order_by
    , id : OptionalArgument Api.Enum.Order_by.Order_by
    , name : OptionalArgument Api.Enum.Order_by.Order_by
    , updated_at : OptionalArgument Api.Enum.Order_by.Order_by
    }


{-| Type for the Games\_max\_order\_by input object.
-}
type alias Games_max_order_by =
    { created_at : OptionalArgument Api.Enum.Order_by.Order_by
    , id : OptionalArgument Api.Enum.Order_by.Order_by
    , name : OptionalArgument Api.Enum.Order_by.Order_by
    , updated_at : OptionalArgument Api.Enum.Order_by.Order_by
    }


{-| Encode a Games\_max\_order\_by into a value that can be used as an argument.
-}
encodeGames_max_order_by : Games_max_order_by -> Value
encodeGames_max_order_by input =
    Encode.maybeObject
        [ ( "created_at", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.created_at ), ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ), ( "name", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.name ), ( "updated_at", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.updated_at ) ]


buildGames_min_order_by : (Games_min_order_byOptionalFields -> Games_min_order_byOptionalFields) -> Games_min_order_by
buildGames_min_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { created_at = Absent, id = Absent, name = Absent, updated_at = Absent }
    in
    { created_at = optionals.created_at, id = optionals.id, name = optionals.name, updated_at = optionals.updated_at }


type alias Games_min_order_byOptionalFields =
    { created_at : OptionalArgument Api.Enum.Order_by.Order_by
    , id : OptionalArgument Api.Enum.Order_by.Order_by
    , name : OptionalArgument Api.Enum.Order_by.Order_by
    , updated_at : OptionalArgument Api.Enum.Order_by.Order_by
    }


{-| Type for the Games\_min\_order\_by input object.
-}
type alias Games_min_order_by =
    { created_at : OptionalArgument Api.Enum.Order_by.Order_by
    , id : OptionalArgument Api.Enum.Order_by.Order_by
    , name : OptionalArgument Api.Enum.Order_by.Order_by
    , updated_at : OptionalArgument Api.Enum.Order_by.Order_by
    }


{-| Encode a Games\_min\_order\_by into a value that can be used as an argument.
-}
encodeGames_min_order_by : Games_min_order_by -> Value
encodeGames_min_order_by input =
    Encode.maybeObject
        [ ( "created_at", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.created_at ), ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ), ( "name", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.name ), ( "updated_at", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.updated_at ) ]


buildGames_obj_rel_insert_input : Games_obj_rel_insert_inputRequiredFields -> (Games_obj_rel_insert_inputOptionalFields -> Games_obj_rel_insert_inputOptionalFields) -> Games_obj_rel_insert_input
buildGames_obj_rel_insert_input required fillOptionals =
    let
        optionals =
            fillOptionals
                { on_conflict = Absent }
    in
    Games_obj_rel_insert_input { data = required.data, on_conflict = optionals.on_conflict }


type alias Games_obj_rel_insert_inputRequiredFields =
    { data : Games_insert_input }


type alias Games_obj_rel_insert_inputOptionalFields =
    { on_conflict : OptionalArgument Games_on_conflict }


{-| Type alias for the `Games_obj_rel_insert_input` attributes. Note that this type
needs to use the `Games_obj_rel_insert_input` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias Games_obj_rel_insert_inputRaw =
    { data : Games_insert_input
    , on_conflict : OptionalArgument Games_on_conflict
    }


{-| Type for the Games\_obj\_rel\_insert\_input input object.
-}
type Games_obj_rel_insert_input
    = Games_obj_rel_insert_input Games_obj_rel_insert_inputRaw


{-| Encode a Games\_obj\_rel\_insert\_input into a value that can be used as an argument.
-}
encodeGames_obj_rel_insert_input : Games_obj_rel_insert_input -> Value
encodeGames_obj_rel_insert_input (Games_obj_rel_insert_input input) =
    Encode.maybeObject
        [ ( "data", encodeGames_insert_input input.data |> Just ), ( "on_conflict", encodeGames_on_conflict |> Encode.optional input.on_conflict ) ]


buildGames_on_conflict : Games_on_conflictRequiredFields -> (Games_on_conflictOptionalFields -> Games_on_conflictOptionalFields) -> Games_on_conflict
buildGames_on_conflict required fillOptionals =
    let
        optionals =
            fillOptionals
                { where_ = Absent }
    in
    Games_on_conflict { constraint = required.constraint, update_columns = required.update_columns, where_ = optionals.where_ }


type alias Games_on_conflictRequiredFields =
    { constraint : Api.Enum.Games_constraint.Games_constraint
    , update_columns : List Api.Enum.Games_update_column.Games_update_column
    }


type alias Games_on_conflictOptionalFields =
    { where_ : OptionalArgument Games_bool_exp }


{-| Type alias for the `Games_on_conflict` attributes. Note that this type
needs to use the `Games_on_conflict` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias Games_on_conflictRaw =
    { constraint : Api.Enum.Games_constraint.Games_constraint
    , update_columns : List Api.Enum.Games_update_column.Games_update_column
    , where_ : OptionalArgument Games_bool_exp
    }


{-| Type for the Games\_on\_conflict input object.
-}
type Games_on_conflict
    = Games_on_conflict Games_on_conflictRaw


{-| Encode a Games\_on\_conflict into a value that can be used as an argument.
-}
encodeGames_on_conflict : Games_on_conflict -> Value
encodeGames_on_conflict (Games_on_conflict input) =
    Encode.maybeObject
        [ ( "constraint", Encode.enum Api.Enum.Games_constraint.toString input.constraint |> Just ), ( "update_columns", (Encode.enum Api.Enum.Games_update_column.toString |> Encode.list) input.update_columns |> Just ), ( "where", encodeGames_bool_exp |> Encode.optional input.where_ ) ]


buildGames_order_by : (Games_order_byOptionalFields -> Games_order_byOptionalFields) -> Games_order_by
buildGames_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { active = Absent, created_at = Absent, id = Absent, name = Absent, updated_at = Absent }
    in
    { active = optionals.active, created_at = optionals.created_at, id = optionals.id, name = optionals.name, updated_at = optionals.updated_at }


type alias Games_order_byOptionalFields =
    { active : OptionalArgument Api.Enum.Order_by.Order_by
    , created_at : OptionalArgument Api.Enum.Order_by.Order_by
    , id : OptionalArgument Api.Enum.Order_by.Order_by
    , name : OptionalArgument Api.Enum.Order_by.Order_by
    , updated_at : OptionalArgument Api.Enum.Order_by.Order_by
    }


{-| Type for the Games\_order\_by input object.
-}
type alias Games_order_by =
    { active : OptionalArgument Api.Enum.Order_by.Order_by
    , created_at : OptionalArgument Api.Enum.Order_by.Order_by
    , id : OptionalArgument Api.Enum.Order_by.Order_by
    , name : OptionalArgument Api.Enum.Order_by.Order_by
    , updated_at : OptionalArgument Api.Enum.Order_by.Order_by
    }


{-| Encode a Games\_order\_by into a value that can be used as an argument.
-}
encodeGames_order_by : Games_order_by -> Value
encodeGames_order_by input =
    Encode.maybeObject
        [ ( "active", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.active ), ( "created_at", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.created_at ), ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ), ( "name", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.name ), ( "updated_at", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.updated_at ) ]


buildGames_set_input : (Games_set_inputOptionalFields -> Games_set_inputOptionalFields) -> Games_set_input
buildGames_set_input fillOptionals =
    let
        optionals =
            fillOptionals
                { active = Absent, created_at = Absent, id = Absent, name = Absent, updated_at = Absent }
    in
    { active = optionals.active, created_at = optionals.created_at, id = optionals.id, name = optionals.name, updated_at = optionals.updated_at }


type alias Games_set_inputOptionalFields =
    { active : OptionalArgument Bool
    , created_at : OptionalArgument Api.ScalarCodecs.Timestamptz
    , id : OptionalArgument Int
    , name : OptionalArgument String
    , updated_at : OptionalArgument Api.ScalarCodecs.Timestamptz
    }


{-| Type for the Games\_set\_input input object.
-}
type alias Games_set_input =
    { active : OptionalArgument Bool
    , created_at : OptionalArgument Api.ScalarCodecs.Timestamptz
    , id : OptionalArgument Int
    , name : OptionalArgument String
    , updated_at : OptionalArgument Api.ScalarCodecs.Timestamptz
    }


{-| Encode a Games\_set\_input into a value that can be used as an argument.
-}
encodeGames_set_input : Games_set_input -> Value
encodeGames_set_input input =
    Encode.maybeObject
        [ ( "active", Encode.bool |> Encode.optional input.active ), ( "created_at", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.created_at ), ( "id", Encode.int |> Encode.optional input.id ), ( "name", Encode.string |> Encode.optional input.name ), ( "updated_at", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.updated_at ) ]


buildGames_stddev_order_by : (Games_stddev_order_byOptionalFields -> Games_stddev_order_byOptionalFields) -> Games_stddev_order_by
buildGames_stddev_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_stddev_order_byOptionalFields =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Type for the Games\_stddev\_order\_by input object.
-}
type alias Games_stddev_order_by =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Encode a Games\_stddev\_order\_by into a value that can be used as an argument.
-}
encodeGames_stddev_order_by : Games_stddev_order_by -> Value
encodeGames_stddev_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ) ]


buildGames_stddev_pop_order_by : (Games_stddev_pop_order_byOptionalFields -> Games_stddev_pop_order_byOptionalFields) -> Games_stddev_pop_order_by
buildGames_stddev_pop_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_stddev_pop_order_byOptionalFields =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Type for the Games\_stddev\_pop\_order\_by input object.
-}
type alias Games_stddev_pop_order_by =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Encode a Games\_stddev\_pop\_order\_by into a value that can be used as an argument.
-}
encodeGames_stddev_pop_order_by : Games_stddev_pop_order_by -> Value
encodeGames_stddev_pop_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ) ]


buildGames_stddev_samp_order_by : (Games_stddev_samp_order_byOptionalFields -> Games_stddev_samp_order_byOptionalFields) -> Games_stddev_samp_order_by
buildGames_stddev_samp_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_stddev_samp_order_byOptionalFields =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Type for the Games\_stddev\_samp\_order\_by input object.
-}
type alias Games_stddev_samp_order_by =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Encode a Games\_stddev\_samp\_order\_by into a value that can be used as an argument.
-}
encodeGames_stddev_samp_order_by : Games_stddev_samp_order_by -> Value
encodeGames_stddev_samp_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ) ]


buildGames_sum_order_by : (Games_sum_order_byOptionalFields -> Games_sum_order_byOptionalFields) -> Games_sum_order_by
buildGames_sum_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_sum_order_byOptionalFields =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Type for the Games\_sum\_order\_by input object.
-}
type alias Games_sum_order_by =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Encode a Games\_sum\_order\_by into a value that can be used as an argument.
-}
encodeGames_sum_order_by : Games_sum_order_by -> Value
encodeGames_sum_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ) ]


buildGames_var_pop_order_by : (Games_var_pop_order_byOptionalFields -> Games_var_pop_order_byOptionalFields) -> Games_var_pop_order_by
buildGames_var_pop_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_var_pop_order_byOptionalFields =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Type for the Games\_var\_pop\_order\_by input object.
-}
type alias Games_var_pop_order_by =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Encode a Games\_var\_pop\_order\_by into a value that can be used as an argument.
-}
encodeGames_var_pop_order_by : Games_var_pop_order_by -> Value
encodeGames_var_pop_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ) ]


buildGames_var_samp_order_by : (Games_var_samp_order_byOptionalFields -> Games_var_samp_order_byOptionalFields) -> Games_var_samp_order_by
buildGames_var_samp_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_var_samp_order_byOptionalFields =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Type for the Games\_var\_samp\_order\_by input object.
-}
type alias Games_var_samp_order_by =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Encode a Games\_var\_samp\_order\_by into a value that can be used as an argument.
-}
encodeGames_var_samp_order_by : Games_var_samp_order_by -> Value
encodeGames_var_samp_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ) ]


buildGames_variance_order_by : (Games_variance_order_byOptionalFields -> Games_variance_order_byOptionalFields) -> Games_variance_order_by
buildGames_variance_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias Games_variance_order_byOptionalFields =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Type for the Games\_variance\_order\_by input object.
-}
type alias Games_variance_order_by =
    { id : OptionalArgument Api.Enum.Order_by.Order_by }


{-| Encode a Games\_variance\_order\_by into a value that can be used as an argument.
-}
encodeGames_variance_order_by : Games_variance_order_by -> Value
encodeGames_variance_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum Api.Enum.Order_by.toString |> Encode.optional input.id ) ]


buildInt_comparison_exp : (Int_comparison_expOptionalFields -> Int_comparison_expOptionalFields) -> Int_comparison_exp
buildInt_comparison_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { eq_ = Absent, gt_ = Absent, gte_ = Absent, in_ = Absent, is_null_ = Absent, lt_ = Absent, lte_ = Absent, neq_ = Absent, nin_ = Absent }
    in
    { eq_ = optionals.eq_, gt_ = optionals.gt_, gte_ = optionals.gte_, in_ = optionals.in_, is_null_ = optionals.is_null_, lt_ = optionals.lt_, lte_ = optionals.lte_, neq_ = optionals.neq_, nin_ = optionals.nin_ }


type alias Int_comparison_expOptionalFields =
    { eq_ : OptionalArgument Int
    , gt_ : OptionalArgument Int
    , gte_ : OptionalArgument Int
    , in_ : OptionalArgument (List Int)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument Int
    , lte_ : OptionalArgument Int
    , neq_ : OptionalArgument Int
    , nin_ : OptionalArgument (List Int)
    }


{-| Type for the Int\_comparison\_exp input object.
-}
type alias Int_comparison_exp =
    { eq_ : OptionalArgument Int
    , gt_ : OptionalArgument Int
    , gte_ : OptionalArgument Int
    , in_ : OptionalArgument (List Int)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument Int
    , lte_ : OptionalArgument Int
    , neq_ : OptionalArgument Int
    , nin_ : OptionalArgument (List Int)
    }


{-| Encode a Int\_comparison\_exp into a value that can be used as an argument.
-}
encodeInt_comparison_exp : Int_comparison_exp -> Value
encodeInt_comparison_exp input =
    Encode.maybeObject
        [ ( "_eq", Encode.int |> Encode.optional input.eq_ ), ( "_gt", Encode.int |> Encode.optional input.gt_ ), ( "_gte", Encode.int |> Encode.optional input.gte_ ), ( "_in", (Encode.int |> Encode.list) |> Encode.optional input.in_ ), ( "_is_null", Encode.bool |> Encode.optional input.is_null_ ), ( "_lt", Encode.int |> Encode.optional input.lt_ ), ( "_lte", Encode.int |> Encode.optional input.lte_ ), ( "_neq", Encode.int |> Encode.optional input.neq_ ), ( "_nin", (Encode.int |> Encode.list) |> Encode.optional input.nin_ ) ]


buildString_comparison_exp : (String_comparison_expOptionalFields -> String_comparison_expOptionalFields) -> String_comparison_exp
buildString_comparison_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { eq_ = Absent, gt_ = Absent, gte_ = Absent, ilike_ = Absent, in_ = Absent, is_null_ = Absent, like_ = Absent, lt_ = Absent, lte_ = Absent, neq_ = Absent, nilike_ = Absent, nin_ = Absent, nlike_ = Absent, nsimilar_ = Absent, similar_ = Absent }
    in
    { eq_ = optionals.eq_, gt_ = optionals.gt_, gte_ = optionals.gte_, ilike_ = optionals.ilike_, in_ = optionals.in_, is_null_ = optionals.is_null_, like_ = optionals.like_, lt_ = optionals.lt_, lte_ = optionals.lte_, neq_ = optionals.neq_, nilike_ = optionals.nilike_, nin_ = optionals.nin_, nlike_ = optionals.nlike_, nsimilar_ = optionals.nsimilar_, similar_ = optionals.similar_ }


type alias String_comparison_expOptionalFields =
    { eq_ : OptionalArgument String
    , gt_ : OptionalArgument String
    , gte_ : OptionalArgument String
    , ilike_ : OptionalArgument String
    , in_ : OptionalArgument (List String)
    , is_null_ : OptionalArgument Bool
    , like_ : OptionalArgument String
    , lt_ : OptionalArgument String
    , lte_ : OptionalArgument String
    , neq_ : OptionalArgument String
    , nilike_ : OptionalArgument String
    , nin_ : OptionalArgument (List String)
    , nlike_ : OptionalArgument String
    , nsimilar_ : OptionalArgument String
    , similar_ : OptionalArgument String
    }


{-| Type for the String\_comparison\_exp input object.
-}
type alias String_comparison_exp =
    { eq_ : OptionalArgument String
    , gt_ : OptionalArgument String
    , gte_ : OptionalArgument String
    , ilike_ : OptionalArgument String
    , in_ : OptionalArgument (List String)
    , is_null_ : OptionalArgument Bool
    , like_ : OptionalArgument String
    , lt_ : OptionalArgument String
    , lte_ : OptionalArgument String
    , neq_ : OptionalArgument String
    , nilike_ : OptionalArgument String
    , nin_ : OptionalArgument (List String)
    , nlike_ : OptionalArgument String
    , nsimilar_ : OptionalArgument String
    , similar_ : OptionalArgument String
    }


{-| Encode a String\_comparison\_exp into a value that can be used as an argument.
-}
encodeString_comparison_exp : String_comparison_exp -> Value
encodeString_comparison_exp input =
    Encode.maybeObject
        [ ( "_eq", Encode.string |> Encode.optional input.eq_ ), ( "_gt", Encode.string |> Encode.optional input.gt_ ), ( "_gte", Encode.string |> Encode.optional input.gte_ ), ( "_ilike", Encode.string |> Encode.optional input.ilike_ ), ( "_in", (Encode.string |> Encode.list) |> Encode.optional input.in_ ), ( "_is_null", Encode.bool |> Encode.optional input.is_null_ ), ( "_like", Encode.string |> Encode.optional input.like_ ), ( "_lt", Encode.string |> Encode.optional input.lt_ ), ( "_lte", Encode.string |> Encode.optional input.lte_ ), ( "_neq", Encode.string |> Encode.optional input.neq_ ), ( "_nilike", Encode.string |> Encode.optional input.nilike_ ), ( "_nin", (Encode.string |> Encode.list) |> Encode.optional input.nin_ ), ( "_nlike", Encode.string |> Encode.optional input.nlike_ ), ( "_nsimilar", Encode.string |> Encode.optional input.nsimilar_ ), ( "_similar", Encode.string |> Encode.optional input.similar_ ) ]


buildTimestamptz_comparison_exp : (Timestamptz_comparison_expOptionalFields -> Timestamptz_comparison_expOptionalFields) -> Timestamptz_comparison_exp
buildTimestamptz_comparison_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { eq_ = Absent, gt_ = Absent, gte_ = Absent, in_ = Absent, is_null_ = Absent, lt_ = Absent, lte_ = Absent, neq_ = Absent, nin_ = Absent }
    in
    { eq_ = optionals.eq_, gt_ = optionals.gt_, gte_ = optionals.gte_, in_ = optionals.in_, is_null_ = optionals.is_null_, lt_ = optionals.lt_, lte_ = optionals.lte_, neq_ = optionals.neq_, nin_ = optionals.nin_ }


type alias Timestamptz_comparison_expOptionalFields =
    { eq_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , gt_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , gte_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , in_ : OptionalArgument (List Api.ScalarCodecs.Timestamptz)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , lte_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , neq_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , nin_ : OptionalArgument (List Api.ScalarCodecs.Timestamptz)
    }


{-| Type for the Timestamptz\_comparison\_exp input object.
-}
type alias Timestamptz_comparison_exp =
    { eq_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , gt_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , gte_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , in_ : OptionalArgument (List Api.ScalarCodecs.Timestamptz)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , lte_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , neq_ : OptionalArgument Api.ScalarCodecs.Timestamptz
    , nin_ : OptionalArgument (List Api.ScalarCodecs.Timestamptz)
    }


{-| Encode a Timestamptz\_comparison\_exp into a value that can be used as an argument.
-}
encodeTimestamptz_comparison_exp : Timestamptz_comparison_exp -> Value
encodeTimestamptz_comparison_exp input =
    Encode.maybeObject
        [ ( "_eq", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.eq_ ), ( "_gt", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.gt_ ), ( "_gte", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.gte_ ), ( "_in", ((Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.list) |> Encode.optional input.in_ ), ( "_is_null", Encode.bool |> Encode.optional input.is_null_ ), ( "_lt", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.lt_ ), ( "_lte", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.lte_ ), ( "_neq", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.optional input.neq_ ), ( "_nin", ((Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecTimestamptz) |> Encode.list) |> Encode.optional input.nin_ ) ]
