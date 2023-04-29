module TemplateFunctions

open Calculator

// 普通のあみだくじ
let normalAmidakuji verticalMin verticalStep verticalMax =
    let calc = calculator (verticalMin, verticalStep, verticalMax, 300000, 22, 3., options ())
    calc.calculate()
    printfn $"%02d{calc.elapsed.Minutes}:%02d{calc.elapsed.Seconds}"
    
// 左右接続あみだくじ
let connectedAmidakuji verticalMin verticalStep verticalMax =
    let calc = calculator (verticalMin, verticalStep, verticalMax, 300000, 22, 3., options true)
    calc.calculate()
    printfn $"%02d{calc.elapsed.Minutes}:%02d{calc.elapsed.Seconds}"