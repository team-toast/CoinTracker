#load "UnixTime.fsx"

open UnixTime

let startDate = toEpochTime 2021 01 01
let endDate = now ()

let startingCollateral = 1000.0
let startingDebt = 2000.0
let targetRatio = 3.0
let upperRatio = 3.5
let lowerRatio = 2.5
let gasPrice = 150.0 // 150.0
let defiSaverFee = 0.003
let exchangeFee = 0.003

let ratioIncrement = 0.05
let lower = 1.7
let upper = 5.0
let upperBounds = [|lower + 2.0 * ratioIncrement .. ratioIncrement .. upper|]
let upperTargets = [|lower + ratioIncrement .. ratioIncrement .. upper - ratioIncrement|]
let lowerTargets = [|lower + ratioIncrement .. ratioIncrement .. upper - ratioIncrement|]
let lowerBounds = [|lower .. ratioIncrement .. upper - 2.0 * ratioIncrement|]

