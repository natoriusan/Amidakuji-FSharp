module Calculator
open System
open System.IO


type options (connectLeftAndRight, duplicationTimes, reverseVertically, reverseHorizontally, horizontalProbability, pathToResultFile) = // vertically -> 上下反転, horizontally -> 左右反転
    new () = options (false, 1, false, false, [||])
    new (connectLeftAndRight) = options (connectLeftAndRight, 1, false, false, [||])
    new (horizontalProbability) = options (false, 1, false, false, horizontalProbability)
    new (connectLeftAndRight, horizontalProbability) = options (connectLeftAndRight, 1, false, false, horizontalProbability)
    new (duplicationTimes, reverseVertically, reverseHorizontally) = options (false, duplicationTimes, reverseVertically, reverseHorizontally, [||])
    new (connectLeftAndRight, duplicationTimes, reverseVertically, reverseHorizontally, horizontalProbability) = options (connectLeftAndRight, duplicationTimes, reverseVertically, reverseHorizontally, horizontalProbability, Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + "/Output/Amidakuji/data.csv")
    
    member this.connectLeftAndRight = connectLeftAndRight
    member this.duplicationTimes = duplicationTimes
    member this.reverseVertically = reverseVertically
    member this.reverseHorizontally = reverseHorizontally
    member this.horizontalProbability = horizontalProbability
    member this.pathToResultFile = pathToResultFile
    
    
// TODO: reverse と horizontalProbability が同時に設定されているときにエラーを出す
type calculator (verticalMin, verticalStep, verticalMax, count, accuracy, accBase, opt: options) =
    let mutable _result = [||]
    let mutable _elapsed = TimeSpan ()
    
    do if opt.connectLeftAndRight && (opt.reverseHorizontally || opt.reverseVertically) then failwithf "reverseHorizontally and reverseVertically can't be true if connectLeftAndRight is true."
       
       if opt.horizontalProbability <> [||] && verticalMin <> verticalMax then failwithf "horizontalProbability can be set when verticalMin equals verticalMax."
    
    new (verticalMin, verticalStep, verticalMax, count) = calculator (verticalMin, verticalStep, verticalMax, count, 18, 3.)
    new (verticalMin, verticalStep, verticalMax, count, opt) = calculator (verticalMin, verticalStep, verticalMax, count, 18, 3., opt)
    new (verticalMin, verticalStep, verticalMax, count, accuracy, accBase) = calculator (verticalMin, verticalStep, verticalMax, count, accuracy, accBase, options ())
    
    
    member this.evaluate verticalLine horizontalLine startFrom =
        let r = Random()
        let calculated =
            let probArr =
                if opt.horizontalProbability = [||] then
                    [| 0 |]
                else
                    [|
                        for i in 0..opt.horizontalProbability.Length-1 ->
                            Array.replicate opt.horizontalProbability[i] i
                    |] |> Array.concat
            let length = probArr.Length
            [|
                for _ in 1..count ->
                    let mutable s = [| 0..verticalLine-1 |]
                    for _ in 1..horizontalLine do
                        if opt.connectLeftAndRight then
                            let line = r.Next(0, verticalLine)
                            let pass = probArr[r.Next(0, length-1)]
                            let a = s[line]
                            s[line] <- s[(line+pass+1)%verticalLine]
                            s[(line+pass+1)%verticalLine] <- a
                        else
                            // TODO: horizontalProbabilityを実装
                            let line = r.Next(0, verticalLine-1)
                            let a = s[line]
                            s[line] <- s[line+1]
                            s[line+1] <- a
                    s |> Array.findIndex ((=)startFrom)
                    // [|
                    //     for _ in 1..horizontalLine ->
                    //         r.Next(0, if opt.connectLeftAndRight then verticalLine else verticalLine-1),
                    //         probArr[r.Next(0, length-1)]
                    // |]
                    // let baseHorizontal =
                    //     [|
                    //         for _ in 1..horizontalLine/opt.duplicationTimes ->
                    //             r.Next(0, if opt.connectLeftAndRight then verticalLine else verticalLine-1)
                    //     |]
                    // [|
                    //     for i in 1..opt.duplicationTimes ->
                    //         if i % 2 = 0 then
                    //             baseHorizontal |> (if opt.reverseVertically then Array.rev else id) |> (if opt.reverseHorizontally then Array.map (fun x -> verticalLine-2-x) else id)
                    //         else
                    //             baseHorizontal
                    // |]
                    //     |> Array.concat
                        // |> calc 
                        // |> Array.findIndex ((=)startFrom)
            |]
            
        [|
            for i in 0..verticalLine-1 ->
                calculated |> Array.filter ((=)i) |> Array.length
        |]
        
    member this.calculate () =
        let sw = Diagnostics.Stopwatch()
        sw.Start()
        File.WriteAllText(opt.pathToResultFile, "x,y\n")
        
        _result <-
            [|
                for i in verticalMin..verticalStep..verticalMax ->
                    async {
                        let mutable vert = 0
                        
                        let mutable lastIndex = 0.
                        let mutable index = 1.
                        for _ in 0..accuracy-1 do
                            let hrz = accBase**index |> int
                            let r = this.evaluate i hrz 0
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
                        File.AppendAllText(opt.pathToResultFile, $"{i},{vert}\n")
                        return vert
                    }
            |] |> Async.Parallel |> Async.RunSynchronously
        
        sw.Stop()
        _elapsed <- sw.Elapsed
        
    member this.result  = _result
    member this.elapsed = _elapsed