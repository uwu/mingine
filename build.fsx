#!/usr/bin/env -S dotnet fsi
#r "nuget: Fable.PublishUtils, 2.4.0"

open PublishUtils

let args =
    fsi.CommandLineArgs
    |> Array.skip 1
    |> List.ofArray

match args with
| IgnoreCase "publish-core"::_ ->
    pushFableNuget "src/Mingine.Core/Mingine.Core.fsproj" [] doNothing
| IgnoreCase "publish-physics"::_ ->
    pushFableNuget "src/Mingine.Physics/Mingine.Physics.fsproj" [] doNothing
| IgnoreCase "publish-engine"::_ ->
    pushFableNuget "src/Mingine.Engine/Mingine.Engine.fsproj" [] doNothing
| _ -> ()