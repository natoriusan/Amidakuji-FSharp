open System
open System.IO


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
            for _ in 1..times ->
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
    let sw = Diagnostics.Stopwatch()
    sw.Start()
    File.WriteAllText(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + "/Output/Amidakuji/data.csv", "x,y\n")
    let verticalMin = 3
    let verticalStep = 1
    let verticalMax = 30
    
    let count = 200000
    
    let accuracy = 18
    
    let result =
        [|
             for i in verticalMin..verticalStep..verticalMax ->
                async {
                    let mutable vert = 0
                    
                    let mutable lastIndex = 0.
                    let mutable index = 1.
                    for _ in 0..accuracy-1 do
                        let hrz = 3.**index |> int
                        let r = evaluate i hrz 0 count
                        if Array.forall (fun x -> float(count/i)*0.95<=float(x) && float(x)<=float(count/i)*1.05) r then
                            vert <- hrz
                            index <- (lastIndex+index)/2.
                        else
                            if index%1. = 0. then
                                index <- index+1.
                                lastIndex <- index-1.
                            else
                                let l = index
                                index <- index + (index-lastIndex)/2.
                                lastIndex <- l
                    
                    printfn "%d finished" i
                    return vert
                }
        |] |> Async.Parallel |> Async.RunSynchronously
    
    ([|verticalMin..verticalStep..verticalMax|], result)
        ||> Array.zip
         |> Array.iter (fun (x, y) -> File.AppendAllText(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + "/Output/Amidakuji/data.csv", $"{x},{y}\n"))
    
    sw.Stop()
    printfn "%d:%d" sw.Elapsed.Minutes sw.Elapsed.Seconds
    0