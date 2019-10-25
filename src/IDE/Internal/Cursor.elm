module IDE.Internal.Cursor exposing (Spec, cursor, update)

{-| -}


{-| Same as first but `set` and `get` can go to different worlds
-}
type alias Spec comp getWorld setWorld =
    { get : getWorld -> comp
    , set : comp -> setWorld -> setWorld
    }


{-| Update whole component set, can be used also with `Component.Set` (not only `Singleton`)
-}
update : { get : world -> comp, set : comp -> world -> world } -> (comp -> comp) -> world -> world
update spec f world =
    spec.set (f (spec.get world)) world


{-| Helper function to create nested components
-}



--cursor : Spec subWorld world -> Spec comp subWorld -> Spec comp world


cursor spec2 spec1 =
    { get = spec2.get >> spec1.get
    , set =
        \comp world ->
            spec2.set (spec1.set comp (spec2.get world)) world
    }
