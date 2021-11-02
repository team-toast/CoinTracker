let startingCollateral = 1000.0
let startingDebt = 2000.0
let targetRatio = 3.0
let upperRatio = 3.5
let lowerRatio = 2.5
let gasPrice = 150.0 // 150.0
let defiSaverFee = 0.003
let exchangeFee = 0.003

let ratioIncrement = 0.05
let targetRatioRange = [|1.7 .. ratioIncrement .. 5.0|]
let tolleranceRange = [|0.1 .. ratioIncrement .. 2.5|]
