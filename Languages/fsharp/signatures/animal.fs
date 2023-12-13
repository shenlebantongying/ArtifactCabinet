module signatures.animal

let myfunction x = x+1
 
[<Sealed>]
type animal() =
    member animal.myMethod () =
        printfn "nice"
        
[<Interface>]
type myInterface =
    abstract member method1 : int -> int
    abstract member method2 : string -> unit

        