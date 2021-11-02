[<Struct>]
type Drawdown =
    { Time : int64
    ; PreviousATH : float
    ; Value : float
    ; Drawdown : float
    ; DrawdownPercentage : float
    ; DrawdownPercentageSum : float
    ; MaxPercentage : float
    ; Period : int
    ; MaxPeriod : int   
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
            ; DrawdownPercentageSum = 0.0
            ; MaxPercentage = 0.0
            ; Period = 0
            ; MaxPeriod = 0
            }]
        | head::_ -> 
            [if (valuef item) > head.PreviousATH then
                { head with 
                    Time = time
                    ; PreviousATH = value
                    ; Value = value
                    ; Drawdown = 0.0
                    ; DrawdownPercentage = 0.0
                    ; Period = 0
                }
            else
                let newDrawdown = value - head.PreviousATH
                let newDrawdownPercentage = newDrawdown / head.PreviousATH
                { Time = time
                ; PreviousATH = head.PreviousATH
                ; Value = value 
                ; Drawdown = newDrawdown
                ; DrawdownPercentage = newDrawdownPercentage
                ; DrawdownPercentageSum = head.DrawdownPercentageSum + newDrawdownPercentage
                ; MaxPercentage = min head.MaxPercentage newDrawdownPercentage
                ; Period = head.Period + 1
                ; MaxPeriod = max head.MaxPeriod head.Period
                }
            ]
            @ acc
    
    list |> List.fold update [] 