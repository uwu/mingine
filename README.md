# Mingine
A truly mini physics and game engine for the web.

Very WIP.

## Usage

Install from npm. Have fun.

Docs when its more complete.

Typescript defs should be updated with js api.

## Modules and target envs
| Module          | Content                       | .NET | Fable | JavaScript |
|-----------------|-------------------------------|:----:|:-----:|:----------:|
| Mingine.Core    | Types, units                  |  ✅   |   ✅   |     ❌      |
| Mingine.Physics | Physics, collision, modelling |  ✅   |   ✅   |     ❌      |
| Mingine.Engine  | A game engine for the web     |  ❌   |   ✅   |     ✅      |

## Contributing
This is written in F#. You can use npm scripts for most things.

Publish to NuGet using `FABLE_NUGET_KEY=[api key] ./build.fsx publish`.

Publish to NPM using `(p)npm publish` lol.