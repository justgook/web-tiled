port module Port.BuildRun exposing (build, levelBuild)

import Json.Encode as E
import Tiled
import Tiled.Level exposing (Level)


port build : E.Value -> Cmd msg



--levelBuild : Level -> Cmd msg


levelBuild scripts level =
    [ ( "level", Tiled.encode level )
    , ( "build", E.string scripts.build )
    , ( "run", E.string scripts.run )
    ]
        |> E.object
        |> build
