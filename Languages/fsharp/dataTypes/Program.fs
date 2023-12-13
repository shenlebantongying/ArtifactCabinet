// Learn more about F# at http://fsharp.org

open System
    
(* Records *)
type Person = { Name : string; Age : int }

[<EntryPoint>]
let main argv =         
    let john = {Name = "john"; Age = 13}

    
    printfn "Hello World from F#!"
    0 // return an integer exit code
