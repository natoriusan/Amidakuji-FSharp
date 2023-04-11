open Calculator

[<EntryPoint>]
let main _ =
    let calc = calculator(3, 1, 30, 200000, 18, 3., options ())
    calc.calculate()
    printfn $"%02d{calc.elapsed.Minutes}:%02d{calc.elapsed.Seconds}"
    0