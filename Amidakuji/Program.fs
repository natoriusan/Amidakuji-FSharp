open Calculator

[<EntryPoint>]
let main _ =
    let calc = calculator(3, 1, 30, 300000, 22, 3., options ())
    calc.calculate()
    printfn $"%02d{calc.elapsed.Minutes}:%02d{calc.elapsed.Seconds}"
    0