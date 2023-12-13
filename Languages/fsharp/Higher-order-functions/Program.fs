open System

[<EntryPoint>]
let main argv =
    // Folding

    let testList = [1..10]
    let sumlist = List.fold (fun acc elem -> acc + elem) 0 testList
    printfn "%d" sumlist

    let testData =[("cats",3);
                   ("dogs",5);
                   ("mice",3)]
    
    let countPairs = List.fold (fun acc (name,num) -> acc+num) 0 testData
    printfn "number of animals %d" countPairs
    
    
    0 // return an integer exit code
