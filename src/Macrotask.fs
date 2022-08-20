module MiniPhys.Macrotask

// SEE MY GITHUB GIST!!!!!

open Fable.Core
open Fable.Core.JsInterop

[<Global("MessageChannel")>]
let private mchannel: obj -> obj = jsNative

let macrotask cb =
    let mc = createNew mchannel ()
    mc?port1?onmessage <- (fun () -> cb())
    mc?port2?postMessage("")
    ()

let macroloop cb =
    let mutable cancel = false
    
    let rec loop () =
        if not cancel then
            cb ()
            macrotask loop
    
    macrotask loop
    
    fun () -> cancel <- true