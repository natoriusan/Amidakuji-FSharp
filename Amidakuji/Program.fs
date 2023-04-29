open Calculator
open TemplateFunctions

[<EntryPoint>]
let main _ =
    // let calc = calculator(3, 1, 30, 300000, 22, 3., options true)
    let calc = calculator (25, 1, 25, 300000, 22, 3., options (true, [| 1..12 |]))
    calc.calculate()
    printfn $"%02d{calc.elapsed.Minutes}:%02d{calc.elapsed.Seconds}"
    0