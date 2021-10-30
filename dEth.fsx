#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET.Interactive,  2.0.0-preview.8"

#load "Parameters.fsx"
#load "UnixTime.fsx"
#load "AssetPrices.fsx"
#load "Graph.fsx"
#load "Drawdown.fsx"

open FSharp.Data
open FSharp.Stats

open Plotly.NET

open UnixTime
open AssetPrices
open Drawdown
open Graph
open Parameters
open System.Collections.Generic

let show a = printf "%A\n" a

let mutable lastTime = now ()
let showTimeDiffn fn = 
    let start = now ()
    let result = fn ()
    show (now () - start)
    result


type PossiblyUndefined =
    | Yeah of decimal
    | Nah

[<Struct>]
type Vault = 
    { Time : int64
    ; Collateral : decimal
    ; Debt : decimal
    ; Price : decimal   
    ; TargetRatio : decimal
    ; LowerRatio : decimal
    ; UpperRatio : decimal
    ; Rebalances : uint 
    } with
    member this.CollateralValue = this.Collateral * this.Price
    member this.ExcessCollateralValue = this.CollateralValue - this.Debt
    member this.ExcessCollateral = (this.ExcessCollateralValue / this.Price)
    member this.Ratio = 
        match this.Debt with
        | 0M -> Nah
        | _ -> Yeah(this.CollateralValue / this.Debt)
    member this.TargetLeverage = 1M/(this.TargetRatio - 1M) + 1M
    member this.ShouldRebalance = 
        match this.Ratio with
        | Nah -> true
        | Yeah ratio -> ratio < this.LowerRatio || ratio > this.UpperRatio

let rebalance (vault:Vault) gasPrice fee = 
    // todo : add fees (gas, exchange, defisaver, slippage)
    let newCollateralValue = vault.ExcessCollateralValue * vault.TargetLeverage
    let collateralValueDiff = newCollateralValue - vault.CollateralValue
    let collateralExchangeCharge = (collateralValueDiff * fee) / vault.Price |> abs
    let gasFee = 1_500_000M * gasPrice / 1_000_000_000M
    let newCollateral = (newCollateralValue / vault.Price) - collateralExchangeCharge - gasFee
    let newDebt = newCollateralValue * (vault.TargetLeverage - 1M) / vault.TargetLeverage
    { vault with 
          Collateral = max newCollateral 0M
        ; Debt = max newDebt 0M
        ; Rebalances = vault.Rebalances + 1u
    }

let nextVault vault (candle:Candle) = 
    let preRebalanceVault = { vault with Time = candle.Time; Price = candle.Close }
    if preRebalanceVault.ShouldRebalance then
        rebalance preRebalanceVault gasPrice (defiSaverFee + exchangeFee)
    else
        preRebalanceVault

let create collateral time price targetRatio upperRatio lowerRatio = 
    let init = 
        { Time = time
        ; Collateral = collateral
        ; Debt = 2000.0M
        ; Price = price
        ; TargetRatio = targetRatio
        ; LowerRatio = lowerRatio
        ; UpperRatio = upperRatio
        ; Rebalances = 0u
        }
    init

// makes a list of vaults over time that can be anylized
let vaultList startingCollateral targetRatio upperRatio lowerRatio startDate endDate = 
    let update (vaults: Vault list) (candle: Candle)  =
        match vaults with
        | [] -> [create startingCollateral candle.Time candle.Close targetRatio upperRatio lowerRatio]
        | head::tail -> 
            // printf "%A" head
            [nextVault head candle] @ vaults

    //(fun () -> 
    savedEthPrices startDate endDate
    |> Array.fold update []
    //|> showTimeDiffn

let (best, stDev) = 
    [|1.55M .. 0.1M .. 4M|]
    |> Array.Parallel.map (
        fun target ->
            [|0.1M .. 0.01M .. 0.5M|]
            |> Array.Parallel.map (
                fun toll ->
                    let vaults = 
                        vaultList 
                            startingCollateral 
                            target 
                            (target + toll) 
                            (target - toll) 
                            (toEpochTime 2019 11 01) 
                            (now ())
                    let stDev =
                        vaults
                        |> Seq.stDevBy (fun v -> float (v.ExcessCollateral / startingCollateral))
                    (vaults |> List.head), stDev
                )
            )
    |> Array.reduce Array.append
    |> Array.sortByDescending (fun (v, stDev) -> v.ExcessCollateral * (decimal stDev))
    |> Array.head

show (best, stDev)

let vaults = vaultList startingCollateral best.TargetRatio best.UpperRatio best.LowerRatio (toEpochTime 2019 11 01) (now ())

let last l = List.head l
let first l = vaults.[vaults.Length - 1]
let actual = (List.head vaults).ExcessCollateral / vaults.[vaults.Length - 1].ExcessCollateral
let expected = (List.head vaults).Price / vaults.[vaults.Length - 1].Price
let rebalances = (List.head vaults).Rebalances
(actual, expected, actual / expected, rebalances)

let time = 
    vaults 
    |> List.map (fun x -> x.Time |> toDateTime)
    
let actualCollateral = 
    vaults 
    |> List.map (fun x -> x.ExcessCollateral)

let predictedCollateral = 
    let firstPrice = (first vaults).Price
    vaults |> List.map (fun x-> {| Time = x.Time; Value = startingCollateral * x.Price / firstPrice |}) 

let vaultDrawdowns = 
    vaults
    |> List.rev
    |> drawdowns (fun v -> v.Time) (fun v -> v.ExcessCollateral)
    |> List.map (fun x -> x.DrawdownPercentage * 100M)

let predictedDrawdowns = 
    predictedCollateral
    |> List.rev
    |> drawdowns (fun v -> v.Time) (fun v -> v.Value)
    |> List.map (fun x -> x.DrawdownPercentage * 100M)