declare module "mini-phys";

/** An object that works like a vec2 in most functions */
interface Vec2Like {
	x: number;
	y: number;
}

// the Types.fs Vec2 type as used by ALL internal code and
// as returned by mp.v()
/** a 2D vector as used in the engine, with helpful methods */
interface Vec2 extends Vec2Like {
	/** creates the vector of this one plus another */
	add(other: Vec2Like): Vec2
	/** creates the vector of this one subtract another */
	sub(other: Vec2Like): Vec2
	/** creates the opposing vector of this one */
	neg(): Vec2
	/** gets the dot product of this vector plus another */
	dot(other: Vec2Like): number
	/** gets the (magnitude of the) cross product of this vector plus another */
	cross(other: Vec2Like): number
	/** creates the vector of this one multiplied by some scalar */
	scale(scalar: number): Vec2
	/** creates the vector of this one divided by some scalar */
	scdiv(scalar: number): Vec2
}

interface PhysicsObject {}

interface GameObject {}

interface WrappedGO extends GameObject {
	o: GameObject;
}

// just about enough to make it unique
// i do not intend to write typedefs for fable unions
interface Collider {
	"___DONT TOUCH THIS OBJECT___": "defos an accurate collider type" 
}


/** the scene - the top level container of the game */
interface Scene {
	/** how many pixels per meter */
	scale: number;
	/** the styles to place on the container */
	rootStyles: object;
	/**
	 * a Fable HashSet of objects in the scene - dont attempt to use this
	 * @internal
	 * @private
	 */
	objects: Iterable<WrappedGO>;
	/** where the camera is placed */
	renderOffset: Vec2;
	/** how many metres in each direction the canvas is */
	canvasSize: Vec2;
	/** hooks to run after every tick */
	postTickHooks: PostTickHook[];
	
	/** gets all game objects in scene */
	getObjects(): WrappedGO[];
	/** adds a game object to the scene */
	addObject(go: WrappedGO): void;
	/** removes a game object from the scene */
	removeObject(go: WrappedGO): void;
}

/** options used to start an engine running */
type EngineStartOpts = undefined | {
	/** 
	 * the rate to update the engine at
	 * @default 200
	 */
	physicsHz?: number;
	/**
	 * a timestep cap, avoids engine instability at the cost of slowdowns
	 * set to <0 to disable
	 * @default
	 */
	tsCap?: number;
	/**
	 * locks the physics tick to happen on each draw frame - overrides physicsHz
	 * @default false
	 */
	lockPhysicsToRender?: boolean;
}

/** the object containing the engine and controlling it */
interface Engine {
	/** the scene being shown and simulated */
	scene: Scene;
	/** if the engine is or is not currently started */
	running: boolean;
	/** the element in which the engine is mounted, if it is */
	mounted: HTMLElement | undefined;
	/**
	 * used for timing, do not touch
	 * @internal
	 * @private
	 */
	lastTick: number;
	/** 
	 * used for render ticks, do not touch
	 * Fable Dictionary<WrappedGO, HTMLElement>
	 * @internal
	 * @private
	 */
	gObjMountedCache: unknown;
	
	/** starts the engine with given options */
	start(opts?: EngineStartOpts): void;
	/** stops a running engine */
	stop(): void;
	/** mounts the engine in the given element */
	mount(elem: HTMLElement): void;
	/** unmounts the engine and returns the element */
	unmount(): HTMLElement;
}

type PostTickHook = (x: [Scene, number]) => void;

type ForceCalculator = (x: [GameObject, number]) => [Vec2, number];

/** creates a 2d vector */
export function v(x: number, y: number): Vec2;
/** gets the origin vector */
export function vo(): Vec2;

/** inits an engine with the given scene */
export function createEngine(scene: Scene): Engine;

/** creates a scene from a partial one */
export function createScene(partial: {
	scale?: number;
	rootStyles?: object;
	objects?: WrappedGO[];
	renderOffset?: Vec2Like
	canvasSize?: Vec2Like
	postTickHooks?: PostTickHook[]
}): Scene;

/** creates a custom game object, intended to be abstracted by the user */
export function createObject(obj: {
	/** the unique id of this object */
	id: string;
	/** 
	 * the render layer of this object 
	 * @default 1
	 */
	layer?: number;
	/** the vector from the bottom left to the center */
	blOffset: Vec2Like;
	/** 
	 * styles to apply to the element 
	 * @default {}
	 */
	styles?: object;
	/** 
	 * the collider used for collision response
	 * @default NullCollider
	 */
	collider?: Collider;
	/**
	 * position of the object
	 * @default (0, 0)
	 */
	pos?: Vec2Like;
	/**
	 * velocity of the object
	 * @default (0, 0)
	 */
	velocity?: Vec2Like;
	/**
	 * acceleration of the object
	 * @default (0, 0)
	 */
	accel?: Vec2Like;
	/** mass of the object in kg*/
	mass: number;
	/** 
	 * functions that model forces 
	 * @default []
	 */
	forces?: ForceCalculator[];
	// TODO: finish
}): WrappedGO;