// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/signature-files

module signatures.Program
open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
