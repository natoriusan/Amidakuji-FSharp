open Calculator
open GeneticAlgorithm
open TemplateFunctions

[<EntryPoint>]
let main _ =
    // let calc = calculator (30, 1, 30, options (true, (Array.replicate 15 1)))
    // calc.calculate()
    // printfn $"%02d{calc.elapsed.Minutes}:%02d{calc.elapsed.Seconds}"
    
    let calc = calculator (40, 1, 40, options (true, Array.replicate 15 1))
    calc.calculate()
    
    let ga = GAofBiasedAmidakuji (40, 1, 40, 20, 10, options true)
    ga.start ()
    
    printfn "not ga: %d" calc.result[0]
    match ga.result[0] with
    | a, b -> printfn "ga: %d" a
              printfn "ga parameter: %A" b
    0