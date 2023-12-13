// Learn more about F# at http://fsharp.org

open System
(* A type with function *)
type peopleName = {First:string; Last:string} with
    member this.FullName =
        this.First + " " + this.Last
        member this.print =
        printfn "%s" this.FullName


    // A constructor wrote outside of a class
let create first last ={First=first;Last=last}

(* A minimum class & a function "attached to it"*)
type animal(name,age)=
    member this.name = name
    member this.age  = age

let printAnimalAge(dog: animal) = printfn "%d" dog.age

[<EntryPoint>]
let main argv =
    let dog = animal("john", 13)
    printAnimalAge dog
    printfn "access a class %d" dog.age
    
    // Two way to create a object, one is "type" another is more "class"
    let john = {First="john"; Last="nice"}
    let tom = create "tom" "cool!"
    
    john.print
    tom.print
            
    0 // return an integer exit code
