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

## Kudos

http://buildnewgames.com/gamephysics - core structure of the engine

https://dyn4j.org/2010/01/sat/ - the one article that made collision actually happen

https://gamedevelopment.tutsplus.com/tutorials/how-to-create-a-custom-2d-physics-engine-the-basics-and-impulse-resolution--gamedev-6331 -
didn't really use this but still some good notes about when to ignore collisions and about preventing sinking.
