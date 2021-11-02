let make2D fa fb fval fvalDefault sequence =    
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

    let findValue a b =
        let entry = 
            sequence
            |> Array.filter (fun x -> fa x = a && fb x = b)
        
        match entry with
        | [||] -> fvalDefault
        | _ -> entry.[0] |> fval

    let actualSurface =
        Array.init surfaceA.Length 
            (fun x -> 
                Array.init 
                    surfaceB.Length 
                    (fun y -> 
                        findValue surfaceA.[x] surfaceB.[y]))
    
    (surfaceA, surfaceB, actualSurface)