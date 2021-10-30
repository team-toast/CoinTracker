#r "nuget: FSharp.Data"
// #r "nuget: Microsoft.Data.Analysis"
#r "nuget: FSharp.Stats"
#r "nuget: FSharp.Json"
#load "UnixTime.fsx"

open UnixTime;
open FSharp.Data
open FSharp.Stats
open System.IO
open FSharp.Json

let show a = printf "%A\n" a

type PriceStream = JsonProvider<"https://api.binance.com/api/v3/klines?symbol=ETHUSDT&interval=1h">

[<Struct>]
type Candle = 
    { Time : int64
    ; Open : decimal
    ; High:  decimal
    ; Low : decimal
    ; Close: decimal }

let makeCandle (assetRow:(decimal array)) =
    { Time = (int64)assetRow.[0]
    ; Open = assetRow.[1]
    ; High = assetRow.[2]
    ; Low = assetRow.[3]
    ; Close = assetRow.[4] 
    }

let assetHistory startTime symbol interval (limit:int) =
    PriceStream.Load(String.Format("https://api.binance.com/api/v3/klines?symbol={0}&interval={1}&limit={2}&startTime={3}", symbol, interval, limit, startTime))
    
let ethPrices fromTime toTime = 
    let result = 
        [|1000L .. 1000L .. 60000L|]
        |> Array.Parallel.map (fun h -> assetHistory (h |> hours |> ago) "ETHUSDT" "1h" 1000)
        |> Array.reduce Array.append
       
    File.WriteAllText("ethPrices.json", result |> Json.serialize)

    result 
    |> Array.Parallel.map makeCandle
    |> Array.filter (fun x -> x.Time >= fromTime && x.Time <= toTime)
    |> Array.sortBy (fun x -> x.Time)

let savedEthPrices fromTime toTime = 
    PriceStream.Load("ethPrices.json")
    |> Array.Parallel.map makeCandle
    |> Array.filter (fun x -> x.Time >= fromTime && x.Time <= toTime)
    |> Array.sortBy (fun x -> x.Time)