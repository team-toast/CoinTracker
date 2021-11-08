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
    ; UpperTargetRatio : float // the ratio that is targeted when the UpperRatio triggers
    ; LowerTargetRatio : float // the ratio that is targeted when the LowerRatio triggers
    ; Tollerance : float
    ; LowerRatio : float
    ; UpperRatio : float
    ; StartingPrice : float
    }

type RebalanceStatus =
    | None
    | UpperTarget
    | LowerTarget

let targetLeverage ratio =
    1.0 / (ratio - 1.0) + 1.0

let targetRatio leverage =
    1.0 / (leverage - 1.0) + 1.0

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
    member inline this.TargetLeverage = targetLeverage this.VaultSettings.TargetRatio
    member inline this.UpperTargetLeverage = targetLeverage this.VaultSettings.UpperTargetRatio
    member inline this.LowerTargetLeverage = targetLeverage this.VaultSettings.LowerTargetRatio
    member inline this.ShouldRebalance = 
        if this.Debt = 0.0 then 
            RebalanceStatus.UpperTarget
        else if this.Ratio > this.VaultSettings.UpperTargetRatio then
            RebalanceStatus.UpperTarget
        else if this.Ratio < this.VaultSettings.LowerTargetRatio then
            RebalanceStatus.LowerTarget
        else
            RebalanceStatus.None
    member inline this.Profit = this.ExcessCollateral * this.Price - (startingCollateral * this.VaultSettings.StartingPrice) 

let rebalance (vault:Vault) gasPrice fee targetLeverage = 
    // todo : add fees (gas, exchange, defisaver, slippage)
    let newCollateralValue = vault.ExcessCollateralValue * targetLeverage
    let collateralValueDiff = newCollateralValue - vault.CollateralValue
    let collateralExchangeCharge = (collateralValueDiff * fee) / vault.Price |> abs
    let gasFee = 1_500_000.0 * gasPrice / 1_000_000_000.0
    let newCollateral = (newCollateralValue / vault.Price) - collateralExchangeCharge - gasFee
    let newDebt = newCollateralValue / (targetRatio targetLeverage)

    { vault with 
          Collateral = max newCollateral 0.0
        ; Debt = max newDebt 0.0
        ; Rebalances = vault.Rebalances + 1u
    }

let nextVault vault (candle:Candle) = 
    let preRebalanceVault = { vault with Time = candle.Time; Price = candle.Close }
    let fee = defiSaverFee + exchangeFee
    match preRebalanceVault.ShouldRebalance with
        | RebalanceStatus.UpperTarget -> rebalance preRebalanceVault gasPrice fee preRebalanceVault.UpperTargetLeverage
        | RebalanceStatus.LowerTarget -> rebalance preRebalanceVault gasPrice fee preRebalanceVault.LowerTargetLeverage
        | RebalanceStatus.None -> preRebalanceVault

let create 
        collateral 
        time 
        price 
        targetRatio 
        upperTargetRatio
        lowerTargetRatio 
        tollerance 
        upperRatio 
        lowerRatio = 
    let init = 
        { Time = time
        ; Collateral = collateral
        ; Debt = 2000.0
        ; Price = price
        ; Rebalances = 0u
        ; VaultSettings = 
            { TargetRatio = targetRatio
            ; UpperTargetRatio = upperTargetRatio
            ; LowerTargetRatio = lowerTargetRatio
            ; Tollerance = tollerance
            ; UpperRatio = upperRatio
            ; LowerRatio = lowerRatio
            ; StartingPrice = price
            }
        }
    init

let vaultList startingCollateral targetRatio upperTargetRatio lowerTargetRatio tollerance upperRatio lowerRatio candleRange = 
    let update (vaults: Vault list) (candle: Candle)  =
        match vaults with
        | [] -> 
            let initVault = 
                create 
                    startingCollateral 
                    candle.Time 
                    candle.Close 
                    targetRatio 
                    upperTargetRatio
                    lowerTargetRatio
                    tollerance 
                    upperRatio 
                    lowerRatio
            [initVault; nextVault initVault candle]
        | head::tail -> 
            [nextVault head candle] @ vaults

    candleRange
    |> Array.fold update []