module MiniPhys.Engine

open Browser.Dom
open Browser.Types
open Fable.Core.JS
open Fable.Core.JsInterop
open MiniPhys.Types

let inline applyStyles (elem: HTMLElement) (styles: obj) =
    Constructors.Object.assign (elem?style, styles) |> ignore

type EngineWrap = {
    mutable scene: Scene
    mutable running: bool
    mutable mounted: HTMLElement option
    
    mutable start: unit -> unit
    mutable stop: unit -> unit
    mutable mount: HTMLElement -> unit
    mutable unmount: unit -> HTMLElement
}

// as far as i know fable has no way of making a class where members can modify mutable props on itself
// the only way to do so is instance members, which fable fails to attach to the prototype chain
// so yeah thats fun. The jsThis trickery attempted here does not work due to f*king ARROW FUNCTIONS WHY

// so yeah making a useful oop in fable usable from js may be literally impossible without custom js emitting
// well done fable, you played f# devs into using fp properly if they want their code to work with non-f# code.

let createEngine scene = {
        scene = scene
        running = false
        mounted = None
        
        mount = (fun elem ->
            jsThis.mounted <- Some elem
            applyStyles elem jsThis.scene.rootStyles
            )
        
        unmount = (fun () ->
            match jsThis.mounted with
            | Some m ->
                m.remove ()
                jsThis.mounted <- None
                m
            | None -> failwith "Cannot unmount a non-mounted scene"
            )
        
        stop = id
        start = id
    }