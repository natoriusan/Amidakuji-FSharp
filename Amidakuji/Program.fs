open System



let calc (start:int array) (lines:int array) =
    let mutable l = lines
    let mutable s = start
    while l <> [||] do
        // s <-
        //     [|
        //         if l[0] = 0 then [||] else s[..l[0]-1]
        //         [|s[l[0]+1]|]
        //         [|s[l[0]]|]
        //         s[l[0]+2..]
        //     |] |> Array.concat
            
        let a = s[l[0]]
        s[l[0]] <- s[l[0]+1]
        s[l[0]+1] <- a
        
        l <- l[1..]
    s
            
            
let evaluate verticalLine horizontalLine startFrom times =
    let r = Random()
    let calculated =
        [|
            for i in 1..times ->
                printf "\r%d / %d" i times
                [| for _ in 0..horizontalLine-1 -> r.Next(0,verticalLine-1) |]
                    |> calc [| 0..verticalLine-1 |]
                    |> Array.findIndex ((=)startFrom)
        |]
        
    [|
        for i in 0..verticalLine-1 ->
            calculated |> Array.filter ((=)i) |> Array.length
    |]
            

[<EntryPoint>]
let main _ =
    let sw = Diagnostics.Stopwatch()
    sw.Start()
    
    let vertical = 40
    let horizontal = 10000
    let results = evaluate vertical horizontal 0 1000
    
    sw.Stop()
    
    results
        |> Array.map string
        |> String.concat ", "
        |> printfn "\n%s"
    
    let floatResults = Array.map float results
    let average = Array.average floatResults
    let sd = (((Array.map (fun x -> x**2.0) floatResults) |> Array.average) - average**2.0) |> sqrt
    printfn "標準偏差: %f" sd
    printfn "変動係数: %f" (sd/average)
    printfn "実行時間: %A" sw.Elapsed
    0