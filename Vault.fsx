#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET.Interactive,  2.0.0-preview.8"

#load "Parameters.fsx"
#load "UnixTime.fsx"
#load "AssetPrices.fsx"
#load "Graph.fsx"
#load "Drawdown.fsx"
#load "Surface.fsx"

open FSharp.Stats

open AssetPrices
open Parameters

type VaultSettings =
    { TargetRatio : float
    ; Tollerance : float
    ; LowerRatio : float
    ; UpperRatio : float
    ; StartingPrice : float
    }

[<Struct>]
type Vault = 
    { Time : int64
    ; Collateral : float
    ; Debt : float
    ; Price : float   
    ; Rebalances : uint 
    ; VaultSettings : VaultSettings
    } with
    member inline this.CollateralValue = this.Collateral * this.Price
    member inline this.ExcessCollateralValue = this.CollateralValue - this.Debt
    member inline this.ExcessCollateral = (this.ExcessCollateralValue / this.Price)
    member inline this.Ratio = this.CollateralValue / this.Debt
    member inline this.TargetLeverage = 1.0/(this.VaultSettings.TargetRatio - 1.0) + 1.0
    member inline this.ShouldRebalance = this.Debt = 0.0 || this.Ratio < this.VaultSettings.LowerRatio || this.Ratio > this.VaultSettings.UpperRatio
    member inline this.Profit = this.ExcessCollateral * this.Price - (startingCollateral * this.VaultSettings.StartingPrice) 

let rebalance (vault:Vault) gasPrice fee = 
    // todo : add fees (gas, exchange, defisaver, slippage)
    let newCollateralValue = vault.ExcessCollateralValue * vault.TargetLeverage
    let newCollateralValue = (vault.Collateral * vault.Price - vault.Debt) * vault.TargetLeverage
    let collateralValueDiff = newCollateralValue - vault.CollateralValue
    let collateralExchangeCharge = (collateralValueDiff * fee) / vault.Price |> abs
    let gasFee = 1_500_000.0 * gasPrice / 1_000_000_000.0
    let newCollateral = (newCollateralValue / vault.Price) - collateralExchangeCharge - gasFee
    let newDebt = newCollateralValue / vault.VaultSettings.TargetRatio

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

let create collateral time price targetRatio tollerance upperRatio lowerRatio = 
    let init = 
        { Time = time
        ; Collateral = collateral
        ; Debt = 2000.0
        ; Price = price
        ; Rebalances = 0u
        ; VaultSettings = 
            { TargetRatio = targetRatio
            ; Tollerance = tollerance
            ; UpperRatio = upperRatio
            ; LowerRatio = lowerRatio
            ; StartingPrice = price
            }
        }
    init

let vaultList startingCollateral targetRatio tollerance upperRatio lowerRatio candleRange = 
    let update (vaults: Vault list) (candle: Candle)  =
        match vaults with
        | [] -> 
            let initVault = 
                create 
                    startingCollateral 
                    candle.Time 
                    candle.Close 
                    targetRatio 
                    tollerance 
                    upperRatio 
                    lowerRatio
            [initVault; nextVault initVault candle]
        | head::tail -> 
            [nextVault head candle] @ vaults

    candleRange
    |> Array.fold update []