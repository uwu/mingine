# Mingine
A truly mini general purpose physics engine, and game engine for the web.

Very WIP.

## Usage

Install from npm. Have fun.

Docs when its more complete.

Typescript defs should be kept up to date enough.

## Packages
 | Module          | Nuget                                                         | NPM                                             |
|-----------------|---------------------------------------------------------------|-------------------------------------------------|
 | Mingine.Physics | [Mingine.Physics](https://nuget.org/packages/Mingine.Physics) | N/A                                             |
 | Mingine.Engine  | [Mingine.Engine](https://nuget.org/packages/Mingine.Engine)   | [mingine-engine](https://npm.im/mingine-engine) |

## Modules and target envs
| Module          | Content                                     | .NET | Fable | TypeScript |
|-----------------|---------------------------------------------|:----:|:-----:|:----------:|
| Mingine.Physics | Types, units, Physics, collision, modelling |  ✅   |   ✅   |     ❌      |
| Mingine.Engine  | A game engine for the web                   |  ❌   |   ✅   |     ✅      |

## Contributing

This is written in F#. You can use npm scripts for most things.

Publish to NuGet using
 - `FABLE_NUGET_KEY=[api key] ./build.fsx publish-physics`
 - `FABLE_NUGET_KEY=[api key] ./build.fsx publish-engine`

Publish to NPM using `(p)npm publish` lol.

Increment npm pkg manually, do not increment nuget pkg manualy.

## //TODO

 - Add renderers
 - Finish events system
 - Add global timing tools
 - Update TS defs
 - Object grouping system like Unity for collisions
 - n-th dimensional simulation support