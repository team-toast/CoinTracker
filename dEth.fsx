#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET.Interactive,  2.0.0-preview.8"

#load "Parameters.fsx"
#load "UnixTime.fsx"
#load "AssetPrices.fsx"
#load "Graph.fsx"
#load "Drawdown.fsx"
#load "Surface.fsx"

open FSharp.Data
open FSharp.Stats

open Plotly.NET

open UnixTime
open AssetPrices
open Drawdown
open Graph
open Parameters
open Surface
open System.Collections.Generic

module DEth =

    let show a = printf "%A\n" a

    show ("now:", now ())

    let mutable lastTime = now ()
    let showTimeDiffn fn = 
        let start = now ()
        let result = fn ()
        show (now () - start)
        result


    type PossiblyUndefined =
        | Yeah of float
        | Nah

    show 1

    [<Struct>]
    type Vault = 
        { Time : int64
        ; Collateral : float
        ; Debt : float
        ; Price : float   
        ; TargetRatio : float
        ; LowerRatio : float
        ; UpperRatio : float
        ; Rebalances : uint 
        } with
        member inline this.CollateralValue = this.Collateral * this.Price
        member inline this.ExcessCollateralValue = this.CollateralValue - this.Debt
        member inline this.ExcessCollateral = (this.ExcessCollateralValue / this.Price)
        member inline this.Ratio = this.CollateralValue / this.Debt
        member inline this.TargetLeverage = 1.0/(this.TargetRatio - 1.0) + 1.0
        member inline this.ShouldRebalance = this.Debt = 0.0 || this.Ratio < this.LowerRatio || this.Ratio > this.UpperRatio

    show 2

    let rebalance (vault:Vault) gasPrice fee = 
        // todo : add fees (gas, exchange, defisaver, slippage)
        let newCollateralValue = vault.ExcessCollateralValue * vault.TargetLeverage
        let collateralValueDiff = newCollateralValue - vault.CollateralValue
        let collateralExchangeCharge = (collateralValueDiff * fee) / vault.Price |> abs
        let gasFee = 1_500_000.0 * gasPrice / 1_000_000_000.0
        let newCollateral = (newCollateralValue / vault.Price) - collateralExchangeCharge - gasFee
        let newDebt = newCollateralValue * (vault.TargetLeverage - 1.0) / vault.TargetLeverage
        { vault with 
              Collateral = max newCollateral 0.0
            ; Debt = max newDebt 0.0
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
            ; Debt = 2000.0
            ; Price = price
            ; TargetRatio = targetRatio
            ; LowerRatio = lowerRatio
            ; UpperRatio = upperRatio
            ; Rebalances = 0u
            }
        init

    let vaultList startingCollateral targetRatio upperRatio lowerRatio candleRange = 
        let update (vaults: Vault list) (candle: Candle)  =
            match vaults with
            | [] -> [create startingCollateral candle.Time candle.Close targetRatio upperRatio lowerRatio]
            | head::tail -> 
                [nextVault head candle] @ vaults

        candleRange
        |> Array.fold update []

    show 3

    let allPrices = savedEthPrices (toEpochTime 2017 06 01) (now ())
    let savedEthPrices startDate endDate =
        allPrices
        |> Array.filter (fun c -> c.Time >= startDate && c.Time <= endDate)

    let candleRange = savedEthPrices (toEpochTime 2020 01 01) (now ())

    show 4

    let optimizations = 
        [|1.55 .. 0.05 .. 10.0|]
        |> Array.map (
            fun target ->
                [|0.1 .. 0.05 .. 10.0|]
                |> Array.map (
                    fun toll ->
                        let vaults = 
                            vaultList 
                                startingCollateral 
                                target 
                                (target + toll) 
                                (max (target - toll) 1.55)
                                candleRange
                        let stDev =
                            vaults
                            |> Seq.stDevBy (fun v -> v.ExcessCollateral / startingCollateral)
                        (vaults |> List.head), stDev
                    )
                )
        |> Array.reduce Array.append

    let (best, stDev) =  
        optimizations
        |> Array.sortByDescending (fun (v, stDev) -> v.ExcessCollateral) // / stDev)
        |> Array.head

    show (best, stDev)

    let (surfaceX, surfaceY, surfaceZ) = 
        make2D 
            (fun (v, _) -> v.TargetRatio) 
            (fun (v, _) -> v.UpperRatio - v.TargetRatio) 
            (fun (v, stDev) -> v.ExcessCollateral)
            optimizations

    let vaults = vaultList startingCollateral best.TargetRatio best.UpperRatio best.LowerRatio candleRange

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
        |> List.map (fun x -> x.DrawdownPercentage * 100.0)

    let predictedDrawdowns = 
        predictedCollateral
        |> List.rev
        |> drawdowns (fun v -> v.Time) (fun v -> v.Value)
        |> List.map (fun x -> x.DrawdownPercentage * 100.0)

    show ("now:", now ())