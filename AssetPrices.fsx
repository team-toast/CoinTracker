#r "nuget: FSharp.Data"
// #r "nuget: Microsoft.Data.Analysis"
#r "nuget: FSharp.Stats"
#load "UnixTime.fsx"

open UnixTime;
open FSharp.Data
open FSharp.Stats

let show a = printf "%A\n" a

type PriceStream = JsonProvider<"https://api.binance.com/api/v3/klines?symbol=ETHUSDT&interval=1h">

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

let assetHistoryFrom startTime symbol interval (limit:int) =
    PriceStream.Load(String.Format("https://api.binance.com/api/v3/klines?symbol={0}&interval={1}&limit={2}&startTime={3}", symbol, interval, limit, startTime))
    |> Array.map makeCandle

let ethPrices fromTime toTime = 
    [|1000L .. 1000L .. 38000L|]
    |> Array.Parallel.map (fun h -> assetHistoryFrom (h |> hours |> ago) "ETHUSDT" "1h" 1000)
    |> Array.reduce Array.append
    |> Array.sortBy (fun x -> x.Time)
    |> Array.filter (fun x -> x.Time >= fromTime && x.Time <= toTime)