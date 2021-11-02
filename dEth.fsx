#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET.Interactive,  2.0.0-preview.8"

#load "Parameters.fsx"
#load "UnixTime.fsx"
#load "AssetPrices.fsx"
#load "Graph.fsx"
#load "Drawdown.fsx"
#load "Surface.fsx"
#load "Vault.fsx"

open System
open FSharp.Stats
open Plotly.NET

open UnixTime
open AssetPrices
open Drawdown
open Parameters
open Surface
open Vault

module DEth =

    let show a = printf "%A\n" a

    show ("now:", now ())

    let mutable lastTime = now ()
    let showTimeDiffn fn = 
        let start = now ()
        let result = fn ()
        show (now () - start)
        result

    let candleRange = savedEthPrices (toEpochTime 2019 01 01) (now ())

    let optimizations = 
        targetRatioRange
        |> Array.map (
            fun target ->
                tolleranceRange
                |> Array.map (
                    fun tollerance ->
                        let vaults = 
                            vaultList 
                                startingCollateral 
                                target 
                                tollerance
                                (target + tollerance) 
                                (max (target - tollerance) 1.55)
                                candleRange

                        let stDev =
                            vaults
                            |> Seq.stDevBy (fun v -> v.ExcessCollateral / startingCollateral)

                        let drawdowns = 
                            vaults
                            |> drawdowns (fun v -> v.Time) (fun v -> v.Profit)

                        (vaults |> List.head, stDev, drawdowns |> List.head)
                    )
                )
        |> Array.reduce Array.append

    let (best, stDev, drawdown) =  
        optimizations
        |> Array.sortByDescending (
            fun (v, stDev, drawdown) -> (v.ExcessCollateral-startingCollateral)**2.0 / (-1.0) * drawdown.MaxPercentage)
        |> Array.head

    show (best, stDev, drawdown)

    let (surfaceX, surfaceY, surfaceZ) = 
        make2D 
            (fun (v, stDev, drawdown) -> v.VaultSettings.TargetRatio)
            (fun (v, stDev, drawdown) -> v.VaultSettings.Tollerance)
            (fun (v, stDev, drawdown) -> 
                if v.ExcessCollateral > startingCollateral then
                    v.ExcessCollateral
                else
                    0.0)
            0.0
            optimizations

    let (surfaceX2, surfaceY2, surfaceZ2) = 
        make2D 
            (fun (v, stDev, drawdown) -> v.VaultSettings.TargetRatio)
            (fun (v, stDev, drawdown) -> v.VaultSettings.Tollerance)
            (fun (v, stDev, drawdown) -> 
                stDev)
            0.0
            optimizations

    let vaults = 
        vaultList 
            startingCollateral 
            best.VaultSettings.TargetRatio
            best.VaultSettings.Tollerance
            best.VaultSettings.UpperRatio
            best.VaultSettings.LowerRatio
            candleRange

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
        let firstPrice = candleRange.[0].Close
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