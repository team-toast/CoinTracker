#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET.Interactive,  2.0.0-preview.8"

#load "Parameters.fsx"
#load "AssetPrices.fsx"
#load "Graph.fsx"
#load "Drawdown.fsx"
#load "Surface.fsx"
#load "Vault.fsx"
#load "UnixTime.fsx"

open System
open FSharp.Stats
open Plotly.NET

open AssetPrices
open Drawdown
open Parameters
open Surface
open Vault
open UnixTime

module DEth =

    show ("now:", now ())

    let candleRange = 
        //ethPrices startDate endDate
        ethPrices startDate endDate

    let makeVault upperTrigger lowerTrigger upperTarget lowerTarget =
        let vaults = 
            vaultList 
                startingCollateral 
                ((upperTarget + lowerTarget) / 2.0)
                upperTarget
                lowerTarget
                (abs (upperTarget - lowerTarget) / 2.0)
                upperTrigger
                lowerTrigger
                candleRange

        let stDev =
            vaults
            |> Seq.stDevBy (fun v -> v.ExcessCollateral / startingCollateral)

        let drawdowns = 
            vaults
            |> drawdowns (fun v -> v.Time) (fun v -> v.Profit)

        (vaults |> List.head, stDev, drawdowns |> List.head)


    let optimizations = 
        upperBounds
        |> Array.map (
            fun upperTrigger ->
                lowerBounds
                |> Array.map (
                    fun lowerTrigger ->
                        makeVault ((upperTrigger + lowerTrigger) / 2.0) ((upperTrigger + lowerTrigger) / 2.0) upperTrigger lowerTrigger
                    ) 
                )
        |> Array.reduce Array.append

    // let optimizations2 = 
    //     upperBounds
    //     |> Array.map (
    //         fun upperTrigger ->
    //             lowerBounds
    //             |> Array.map (
    //                 fun lowerTrigger ->
    //                     upperTargets
    //                     |> Array.map (
    //                         fun upperTarget -> 
    //                             lowerTargets
    //                             |> Array.map (
    //                                 fun lowerTarget -> 
    //                                     makeVault upperTrigger lowerTrigger upperTarget lowerTarget)
    //                         )
    //                 )
    //         )
    //     |> Array.reduce Array.append
    //     |> Array.reduce Array.append
    //     |> Array.reduce Array.append

    
    let (best, stDev, drawdown) =  
        optimizations
        |> Array.filter (fun (v, _, _) -> v.ExcessCollateral > 1000.00)
        |> Array.sortByDescending (
            fun (v, stDev, drawdown) -> v.ExcessCollateral / (float drawdown.MaxPeriod))
        |> Array.head

    show (best, stDev, drawdown)

    let (surfaceX, surfaceY, surfaceZ) = 
        make2D 
            (fun (v, stDev, drawdown) -> v.VaultSettings.UpperRatio)
            (fun (v, stDev, drawdown) -> v.VaultSettings.LowerRatio)
            (fun (v, stDev, drawdown) -> 
                if v.ExcessCollateral > startingCollateral then
                    v.ExcessCollateral
                else
                    0.0)
            0.0
            optimizations


    let average = 
        optimizations
        |> Array.filter (fun (v, std, drawdown) -> drawdown.DrawdownPercentageSum < 0.0)
        |> Array.averageBy (fun (v, std, drawdown) -> drawdown.DrawdownPercentageSum )
    let (surfaceX2, surfaceY2, surfaceZ2) = 
        make2D 
            (fun (v, stDev, drawdown) -> v.VaultSettings.UpperRatio)
            (fun (v, stDev, drawdown) -> v.VaultSettings.LowerRatio)
            (fun (v, stDev, drawdown) -> 
                if v.ExcessCollateral > startingCollateral then 
                    drawdown.DrawdownPercentageSum
                else 
                    average)
            0.0
            optimizations

    let vaults = 
        vaultList 
            startingCollateral 
            best.VaultSettings.TargetRatio
            best.VaultSettings.UpperTargetRatio
            best.VaultSettings.LowerTargetRatio
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