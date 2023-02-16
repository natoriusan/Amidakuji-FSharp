open System
open Plotly.NET



let calc (start:int array) (lines:int array) =
    let mutable s = start
    for i in lines do
        let a = s[i]
        s[i] <- s[i+1]
        s[i+1] <- a
    s
            
            
let evaluate verticalLine horizontalLine startFrom times =
    let r = Random()
    let calculated =
        [|
            for i in 1..times ->
                [| for _ in 0..horizontalLine-1 -> r.Next(0,verticalLine-1) |]
                    |> calc [| 0..verticalLine-1 |]
                    |> Array.findIndex ((=)startFrom)
        |]
        
    [|
        for i in 0..verticalLine-1 ->
            calculated |> Array.filter ((=)i) |> Array.length
    |]
    
    
let getStandardDeviation (arr: int array) =
    let floatArr = arr |> Array.map float
    ((floatArr |> Array.map (fun x -> x**2) |> Array.average) - (floatArr |> Array.average)**2)
        |> sqrt
            

[<EntryPoint>]
let main _ =
    let vertical = 40
    
    let min = 0
    let max = 10000
    let step = 50
    let count = 10000
    
    let x = [| min..step..max |]
    let y =
        [|
            for i in min..step..max ->
                printf "\r%d / %d" (i/step) (max/step |> int)
                evaluate vertical i 0 count
                    |> (fun x -> getStandardDeviation x / (x |> Array.map float |> Array.average))
        |]
        
    Chart.Point(x,y)
        |> Chart.withXAxisStyle "Number of Horizontal Lines"
        |> Chart.withYAxisStyle "Coefficient of Variation"
        |> Chart.show
        
    0