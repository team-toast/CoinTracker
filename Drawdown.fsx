[<Struct>]
type Drawdown =
    { Time : int64
    ; PreviousATH : float
    ; Value : float
    ; Drawdown : float
    ; DrawdownPercentage : float
    }

let drawdowns timef valuef list = 
    let update acc item =
        let time = timef item
        let value = valuef item
        match acc with
        | [] -> 
            [{ Time = time
            ; PreviousATH = value
            ; Value = value
            ; Drawdown = 0.0
            ; DrawdownPercentage = 0.0
            }]
        | head::_ -> 
            [if (valuef item) > head.PreviousATH then
                { Time = time
                ; PreviousATH = value
                ; Value = value
                ; Drawdown = 0.0
                ; DrawdownPercentage = 0.0
                }
            else
                let newDrawdown = value - head.PreviousATH
                let newDrawdownPercentage = newDrawdown / head.PreviousATH
                { Time = time
                ; PreviousATH = head.PreviousATH
                ; Value = value
                ; Drawdown = newDrawdown
                ; DrawdownPercentage = newDrawdownPercentage 
                }
            ]
            @ acc
    
    list |> List.fold update [] 