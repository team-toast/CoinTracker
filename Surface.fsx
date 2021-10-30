let make2D fa fb fval sequence =    
    let surface = 
        sequence
        |> Array.groupBy fa
        |> Array.sortBy (fun (g,_) -> g)
        |> Array.map (fun (_, items) -> 
            items 
            |> Array.groupBy fb
            |> Array.sortBy (fun (g,_) -> g)
            |> Array.map (fun (_, items) -> items |> Array.head |> fval))

    let surfaceA = 
        sequence
        |> Array.distinctBy fa
        |> Array.map fa
        |> Array.sort

    let surfaceB =
        sequence
        |> Array.distinctBy fb
        |> Array.map fb
        |> Array.sort

    (surface, surfaceA, surfaceB)