#!/usr/bin/env -S dotnet fsi
#r "nuget: Fable.PublishUtils, 2.4.0"

open PublishUtils

let args =
    fsi.CommandLineArgs
    |> Array.skip 1
    |> List.ofArray

match args with
| IgnoreCase "publish"::_ ->
    pushFableNuget "src/Mingine.Core.fsproj" [] doNothing
    pushFableNuget "src/Mingine.Physics.fsproj" [] doNothing
    pushFableNuget "src/Mingine.Engine.fsproj" [] doNothing
| _ -> ()