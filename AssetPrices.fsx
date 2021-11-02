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
open System

let show a = printf "%A\n" a

type PriceStream = JsonProvider<"https://api.binance.com/api/v3/klines?symbol=ETHUSDT&interval=1h">

[<Struct>]
type Candle = 
    { Time : int64
    ; Open : float
    ; High:  float
    ; Low : float
    ; Close: float }

let makeCandle (assetRow:(decimal array)) =
    { Time = (int64)assetRow.[0]
    ; Open = float assetRow.[1]
    ; High = float assetRow.[2]
    ; Low = float assetRow.[3]
    ; Close = float assetRow.[4] 
    }

let assetHistory startTime symbol interval (limit:int) =
    PriceStream.Load(uri = String.Format("https://api.binance.com/api/v3/klines?symbol={0}&interval={1}&limit={2}&startTime={3}", symbol, interval, limit, startTime))
    
let ethPrices fromTime toTime = 
    let result = 
        [|1000L .. 1000L .. 60000L|]
        |> Array.Parallel.map (fun h -> assetHistory (h |> hours |> ago) "ETHUSDT" "1h" 1000)
        |> Array.reduce Array.append
       
    File.WriteAllText("data/ethPrices.json", result |> Json.serialize)

    result 
    |> Array.Parallel.map makeCandle
    |> Array.filter (fun x -> x.Time >= fromTime && x.Time <= toTime)
    |> Array.sortBy (fun x -> x.Time)

let loadEthPrices fromTime toTime = 
    PriceStream.Load("data/ethPrices.json")
    |> Array.Parallel.map makeCandle
    |> Array.filter (fun x -> x.Time >= fromTime && x.Time <= toTime)
    |> Array.sortBy (fun x -> x.Time)

let allPrices = 
    loadEthPrices (toEpochTime 2017 06 01) (now ())
    
let savedEthPrices startDate endDate =
    allPrices
    |> Array.filter (fun c -> c.Time >= startDate && c.Time <= endDate)