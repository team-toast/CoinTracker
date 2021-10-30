[<Struct>]
type Drawdown =
    { Time : int64
    ; PreviousATH : decimal
    ; Value : decimal
    ; Drawdown : decimal
    ; DrawdownPercentage : decimal
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
            ; Drawdown = 0M
            ; DrawdownPercentage = 0M 
            }]
        | head::_ -> 
            [if (valuef item) > head.PreviousATH then
                { Time = time
                ; PreviousATH = value
                ; Value = value
                ; Drawdown = 0M
                ; DrawdownPercentage = 0M 
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