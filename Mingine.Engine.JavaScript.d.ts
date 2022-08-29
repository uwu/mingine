declare module "mingine-engine";

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

	/** returns the length of this vector */
	len(): number;

	/** rotates this vector by the given angle about the given origin */
	rotate(angle: number, origin: Vec2Like): Vec2;

	/** returns this vector normalized (with length of 1) */
	norm(): Vec2;

	/** returns the angle between this vector and another */
	angleTo(other: Vec2Like): number;

	/** gets a perpendicular vector to this one - often useful with neg() */
	perp(): Vec2;
}

/** An object as seen and operated on by the physics engine */
interface PhysicsObject {
	/** how heavy the object is in kilograms */
	mass: number;
	/** the position of the object in meters */
	pos: Vec2;
	/** the velocity of the object in meters/second */
	velocity: Vec2;
	/** the acceleration of the object in meters/second^2 */
	accel: Vec2;

	/** force calculators having an effect on this object */
	forces: ForceCalculator[]

	/** the moment of inertia of this object - see wikipedia! */
	momentOfInertia: number;
	/** the angle CLOCKWISE of the object in radians - be careful with equations that expect opposite! */
	angle: number;
	/** angular velocity in radians/second */
	angVelocity: number;
	/** angular acceleration in radians/second^2 */
	angAccel: number;

	/** the coefficient of restitution (how bouncy an object is!) */
	restitutionCoeff: number;
}

/** Represents a renderable object in the game */
interface GameObject {
	/** the unique ID of this game object */
	id: string;
	/** the physics object for this game object */
	physicsObj: PhysicsObject;
	/** the render layer (z-index) of this object */
	layer: number;
	/** the vector from the bottom left to the center of the object */
	blOffset: Vec2;
	/** the CSS on this object */
	styles: object;
	/** the collider this object is using */
	collider: Collider;
}

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
	 */
	objects: unknown;
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
	 */
	lastTick: number;
	/**
	 * used for render ticks, do not touch
	 * Fable Dictionary<WrappedGO, HTMLElement>
	 * @internal
	 */
	gObjMountedCache: unknown;

	/**
	 * used for collision, do not touch
	 * Fable Dictionary<WrappedGO, WrappedGO list>
	 * @internal
	 */
	collisionCache: unknown
	
	/** checks if two objects collided in this scene last tick */
	queryCollision(o1: WrappedGO, o2: WrappedGO): boolean;
	
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
	/** the amount of pixels per meter */
	scale?: number;
	/** the CSS for the canvas */
	rootStyles?: object;
	/** the objects in the scene */
	objects?: WrappedGO[];
	/** the position of the camera */
	renderOffset?: Vec2Like
	/** how big the canvas is */
	canvasSize?: Vec2Like
	/** hooks to run after each tick*/
	postTickHooks?: PostTickHook[]
}): Scene;

type CustomObjectCreator = {
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
	/** moment of inertia (see wikipedia) */
	momentOfInertia: number;
	/**
	 * the rotation of this object
	 * @default 0
	 */
	angle?: number;
	/**
	 * the angular velocity of this object
	 * @default 0
	 */
	angVelocity?: number;
	/**
	 * the angular acceleration of this object
	 * @default 0
	 */
	angAccel?: number;
	/**
	 * the coefficient of restitution (how bouncy is it!)
	 * @default 1
	 */
	restitutionCoeff?: number;
}

/** creates a custom game object, intended to be abstracted by the user */
export function createObject(obj: CustomObjectCreator): WrappedGO;

type CircleCreator =
	Omit<CustomObjectCreator, "collider" | "blOffset">
	& {
	/** the radius, in meters, of the circle */
	radius: number;
	/**
	 * when true, adds a suitable collider
	 * @default false
	 */
	collide?: boolean;
};

/** creates a circle game object */
export function createCircle(obj: CircleCreator): WrappedGO;

type RectCreator =
	Omit<CustomObjectCreator, "collider" | "blOffset">
	& {
	/** the width, in meters, of the rectangle */
	width: number;
	/** the height, in meters, of the rectangle */
	height: number;
	/**
	 * when true, adds a suitable collider
	 * @default false
	 */
	collide?: boolean;
};

/** creates a rectangular game object */
export function createRect(obj: RectCreator): WrappedGO;

/** creates a "collider" that cannot collide with anything */
export function createColliderNull(): Collider;
/** creates a collider that combines two colliders into one */
export function createColliderComposite(a: Collider, b: Collider): Collider;
/** creates a collider for a circle */
export function createColliderCircle(radius: number, center: Vec2Like): Collider;
/** creates a collider for a rectangle */
export function createColliderRect(bottomLeft: Vec2Like, topRight: Vec2Like): Collider;

/** contains models of real forces */
export const forceModels: {
	/** models gravity */
	weight(gravity: number): ForceCalculator;
	/** models a spring */
	spring(springConst: number, restPos: Vec2Like, connectionOset: Vec2Like): ForceCalculator;
	/** models air resistance */
	airDrag(absoluteFlowVelocity: Vec2Like, density: number, csArea: number, dragCoeff: number): ForceCalculator;
	/** models air resistance in still air */
	stillAirDrag(density: number, csArea: number, dragCoeff: number): ForceCalculator;
	
	/** a super simple damping force (not a real force model!) */
	simpleDamping(positionRatio: number, angleRatio: number): ForceCalculator
};

/** physical constants of earth :) */
export const consts: {
	/** the gravity of earth */
	earthGravity: Vec2;
	/** the air density of earth */
	earthAirDensity: number;
};