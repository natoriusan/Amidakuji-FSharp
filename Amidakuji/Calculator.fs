module Calculator
open System
open System.IO


type options (connectLeftAndRight, duplicationTimes, reverseVertically, reverseHorizontally) = // vertically -> 上下反転, horizontally -> 左右反転
    new () = options (false, 1, false, false)
    new (connectLeftAndRight) = options (connectLeftAndRight, 1, false, false)
    new (duplicationTimes, reverseVertically, reverseHorizontally) = options (false, duplicationTimes, reverseVertically, reverseHorizontally)
    
    member this.connectLeftAndRight = connectLeftAndRight
    member this.duplicationTimes = duplicationTimes
    member this.reverseVertically = reverseVertically
    member this.reverseHorizontally = reverseHorizontally
    
    

type calculator (verticalMin, verticalStep, verticalMax, count, accuracy, accBase, opt: options) =
    let mutable _result = [||]
    let mutable _elapsed = TimeSpan ()
    
    do if opt.connectLeftAndRight && (opt.reverseHorizontally || opt.reverseVertically) then failwithf "reverseHorizontally and reverseVertically can't be true if connectLeftAndRight is true."
    
    new (vm, vs, vM, co) = calculator (vm, vs, vM, co, 18, 3.)
    new (vm, vs, vM, co, op) = calculator (vm, vs, vM, co, 18, 3., op)
    new (vm, vs, vM, co, acc, accB) = calculator (vm, vs, vM, co, acc, accB, options ())
    
    
    member this.evaluate verticalLine horizontalLine startFrom =
        let calc lines =
            let mutable s = [| 0..verticalLine-1 |]
            if opt.connectLeftAndRight then
                for i in lines do
                    let a = s[i]
                    if i = verticalLine-1 then
                        s[i] <- s[0]
                        s[0] <- a
                    else
                        s[i] <- s[i+1]
                        s[i+1] <- a  
            else
                for i in lines do
                    let a = s[i]
                    s[i] <- s[i+1]
                    s[i+1] <- a
            s
            
        let r = Random()
        let calculated =
            [|
                for _ in 1..count ->
                    let baseHorizontal =
                        [|
                            for _ in 1..horizontalLine/opt.duplicationTimes ->
                                r.Next(0, if opt.connectLeftAndRight then verticalLine else verticalLine-1)
                        |]
                    [|
                        for i in 1..opt.duplicationTimes ->
                            if i % 2 = 0 then
                                baseHorizontal |> (if opt.reverseVertically then Array.rev else id) |> (if opt.reverseHorizontally then Array.map (fun x -> verticalLine-2-x) else id)
                            else
                                baseHorizontal
                    |]
                        |> Array.concat
                        |> calc 
                        |> Array.findIndex ((=)startFrom)
            |]
            
        [|
            for i in 0..verticalLine-1 ->
                calculated |> Array.filter ((=)i) |> Array.length
        |]
        
    member this.calculate () =
        let sw = Diagnostics.Stopwatch()
        sw.Start()
        File.WriteAllText(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + "/Output/Amidakuji/data.csv", "x,y\n")
        
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
                        File.AppendAllText(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + "/Output/Amidakuji/data.csv", $"{i},{vert}\n")
                        return vert
                    }
            |] |> Async.Parallel |> Async.RunSynchronously
        
        sw.Stop()
        _elapsed <- sw.Elapsed
        
    member this.result  = _result
    member this.elapsed = _elapsed