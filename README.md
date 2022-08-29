# Mingine
A truly mini physics and game engine for the web.

Very WIP.

## Usage

Install from npm. Have fun.

Docs when its more complete.

Typescript defs should be kept up to date enough.

## Packages
 | Module          | Nuget                                                         | NPM                                             |
|-----------------|---------------------------------------------------------------|-------------------------------------------------|
| Mingine.Core    | [Mingine.Core](https://nuget.org/packages/Mingine.Core)       | N/A                                             |
 | Mingine.Physics | [Mingine.Physics](https://nuget.org/packages/Mingine.Physics) | N/A                                             |
 | Mingine.Engine  | [Mingine.Engine](https://nuget.org/packages/Mingine.Engine)   | [mingine-engine](https://npm.im/mingine-engine) |

## Modules and target envs
| Module          | Content                       | .NET | Fable | TypeScript |
|-----------------|-------------------------------|:----:|:-----:|:----------:|
| Mingine.Core    | Types, units                  |  ✅   |   ✅   |     ❌      |
| Mingine.Physics | Physics, collision, modelling |  ✅   |   ✅   |     ❌      |
| Mingine.Engine  | A game engine for the web     |  ❌   |   ✅   |     ✅      |

## Contributing

This is written in F#. You can use npm scripts for most things.

Publish to NuGet using
 - `FABLE_NUGET_KEY=[api key] ./build.fsx publish-core`
 - `FABLE_NUGET_KEY=[api key] ./build.fsx publish-physics`
 - `FABLE_NUGET_KEY=[api key] ./build.fsx publish-engine`

Publish to NPM using `(p)npm publish` lol.

Increment npm pkg manually, do not increment nuget pkg manualy.

## //TODO

 - Add renderers
 - Add mouse interaction and maybe wider events system
 - Add global timing tools
 - 