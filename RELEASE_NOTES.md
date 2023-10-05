### 1.1.2
- Fix circles having a moment of inertia of 0
- Optimize core vector operations, integrator checks & compiled output (SWC)
- Update to latest Fable (4.x)

### 1.1.1
- Fix broken bundle due to [issue in beta version of Fable](https://github.com/fable-compiler/Fable/issues/3306)

### 1.1.0
- Cleanup engine code using `this`
- Add `Vec2.atAngle` / `mg.vAngle()`
- Add `Simulator.impulse` / `physicsObj.prototype.impulse()`
- Un-expose some internal `Simulator` funcs
- More toString tags on object prototypes, which should improve debugging experience in some browsers
- Fix collision response clipping issue
- Add post-frame hooks
- Add event handling for DOM events
  * Imperatively when they occur on the relevant game object
  * Imperatively when they occur globally
  * All since last tick on the relevant game object
  * All since last tick globally
  * All since last frame globally
- Add infinite plane colliders - great for floors and walls
- Add world colliders, which are not tied to a game object and immovable
- Fix incorrect minning of vectors for collision
- The default styles no longer make objects look like they're clipping when close
- `mg.createCircle()` and `mg.createRect()` now automatically calculate moments of inertia
- Fix 0 masses breaking things
- Impulses can now be applied from offset positions (//TODO: test)

### 1.0.1
- Fix some wrong typedefs
- Fix y values below 0 breaking in `Engine.updateGameObject`
- Add `"type": "module"` to fix some tools (eg Astro CLI) complaining
- Fix target .NET versions for Physics and Engine

### 1.0.0
 - Hello World! Mingine is now on NuGet and NPM.