module signatures.animal
    val myfunction: int -> int
    
    [<Sealed>]
    type animal =
        new : unit -> animal
        member myMethod: unit -> unit
        
    [<Interface>]
    type myInterface =
        abstract member method1 : int -> int
        abstract member method2 : string -> unit
        
        
        
      
        
