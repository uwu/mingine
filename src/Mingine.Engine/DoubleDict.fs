namespace Mingine.Types.Engine

open System.Collections.Generic

/// A two-way hash map
type DoubleDict<'a, 'b> when 'a : equality and 'b : equality () =
    member val forward = Dictionary<'a, 'b>()
    member val backward = Dictionary<'b, 'a>()
    
    member this.Add v1 v2 =
        this.forward.Add(v1, v2)
        this.backward.Add(v2, v1)
        
    member this.Clear() =
        this.forward.Clear()
        this.backward.Clear()
    
    member this.Contains1 v = this.forward.ContainsKey v
    member this.Contains2 v = this.backward.ContainsKey v
    
    interface seq<'a * 'b> with
        member this.GetEnumerator() : IEnumerator<'a * 'b> =
            (this.forward |> Seq.map(fun v -> (v.Key, v.Value))).GetEnumerator()
            
        member this.GetEnumerator() : System.Collections.IEnumerator =
            (this.forward |> Seq.map(fun v -> (v.Key, v.Value))).GetEnumerator()
    member this.Remove1 v1 =
        if this.Contains1 v1 then
            let v2 = this.forward[v1]
            this.forward.Remove v1 |> ignore
            this.backward.Remove v2 |> ignore
            true
        else false
    
    member this.Remove2 v2 =
        if this.Contains2 v2 then
            let v1 = this.backward[v2]
            this.backward.Remove v2 |> ignore
            this.forward.Remove v1 |> ignore
            true
        else false
    
    member this.TryGet1 v =
        let success, value = this.forward.TryGetValue v
        if success then Some value else None
        
    member this.TryGet2 v =
        let success, value = this.backward.TryGetValue v
        if success then Some value else None
    
    member this.Count = this.forward.Count
    
    member this.Values1 = this.forward.Keys
    member this.Values2 = this.backward.Keys
    
    member this.Get1 v = this.forward[v]
    member this.Get2 v = this.backward[v]
    
    member this.Set1 v1 v2 =
        this.forward[v1] <- v2
        this.backward[v2] <- v1
    
    member this.Set2 v2 v1 = this.Set1 v1 v2