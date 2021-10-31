let make2D fa fb fval sequence =    
    let surface = 
        sequence
        |> Array.groupBy fa
        |> Array.sortBy (fun (ga,_) -> ga)
        |> Array.map (fun (_, items) -> 
            items 
            |> Array.groupBy fb
            |> Array.sortBy (fun (gb,_) -> gb)
            |> Array.map (fun (_, items) -> items |> Array.head |> fval))

    let surfaceA = 
        sequence
        |> Array.map fa
        |> Array.distinct
        |> Array.sort

    let surfaceB =
        sequence
        |> Array.map fb
        |> Array.distinct
        |> Array.sort

    (surfaceA, surfaceB, surface)