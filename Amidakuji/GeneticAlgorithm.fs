module GeneticAlgorithm
open System
open Calculator


type GAofBiasedAmidakuji (verticalMin, verticalStep, verticalMax, maxGeneration, individualMax, count, accuracy, accBase, opt: options) =
    let mutable _result = [||]
    let r = Random ()
    let probLevel = 5
    let mutationProb = 0.1
    let copiedOpt =
        let o = opt.copy ()
        o.pathToResultFile <- ""
        o.showProgress <- false
        o
        
    let getFitness verticalLine genes =
        [|
            for i in genes ->
                async {
                    let o = copiedOpt.copy ()
                    o.horizontalProbability <- i
                    let calc = calculator (verticalLine, 1, verticalLine, count, accuracy, accBase, o)
                    calc.calculate ()
                    return calc.result[0], i
                }
        |] |> Async.Parallel |> Async.RunSynchronously
        
    let rec rankingSelect (genes: (int * int array) array) =
        let ranking =
            [|
                for i in 0..genes.Length-1 ->
                    Array.replicate (genes.Length-i) i
            |] |> Array.concat
        let parent1 = genes[ranking[r.Next(0, ranking.Length-1)]]
        let parent2 = genes[ranking[r.Next(0, ranking.Length-1)]]
        
        if parent1 = parent2 then
            rankingSelect genes
        else
            parent1, parent2
        
        
    let uniformCrossover parent1 parent2 =
        let mutable child1 = [||]
        let mutable child2 = [||]
        for a, b in Array.zip parent1 parent2 do
            if r.Next (0, 2) < 1 then
                child1 <- Array.append child1 [|a|]
                child2 <- Array.append child2 [|b|]
            else
                child1 <- Array.append child1 [|b|]
                child2 <- Array.append child2 [|a|]
                
        [| child1; child2 |]
        
    let mutation genes =
        [|
            for gene in genes ->
                [|
                    for i in gene ->
                        if r.NextDouble () < mutationProb then
                            r.Next (0, probLevel)
                        else
                            i
                |]
        |]
        
    
    new (verticalMin, verticalStep, verticalMax, maxGeneration, individualMax) = GAofBiasedAmidakuji (verticalMin, verticalStep, verticalMax, maxGeneration, individualMax, 300000, 22, 3., options ())
    new (verticalMin, verticalStep, verticalMax, maxGeneration, individualMax, opt) = GAofBiasedAmidakuji (verticalMin, verticalStep, verticalMax, maxGeneration, individualMax, 300000, 22, 3., opt)
    new (verticalMin, verticalStep, verticalMax, maxGeneration, individualMax, count, accuracy, accBase) = GAofBiasedAmidakuji (verticalMin, verticalStep, verticalMax, maxGeneration, individualMax, count, accuracy, accBase, options ())
    
    member this.start () =
        _result <-
            [|
                for i in verticalMin..verticalStep..verticalMax ->
                    let mutable genes =
                        [|
                            for _ in 1..individualMax ->
                                [| for _ in 1..(i+1)/2 -> r.Next (0, probLevel) |]
                        |]
                    let mutable lastBest = (0, [||])
                    for j in 1..maxGeneration do
                        let sortedGenes = getFitness i genes |> Array.sort
                        if j = 1 then
                            lastBest <- sortedGenes[0]
                        else
                            let b, _ = lastBest
                            let s, _ = sortedGenes[0]
                            if s < b then
                                lastBest <- sortedGenes[0]
                        genes <-
                            [|
                                for _ in 1..individualMax/2 ->
                                    let (_, a), (_, b) = rankingSelect sortedGenes
                                    uniformCrossover a b
                            |] |> Array.concat |> mutation
                        printfn $"Generation {j} finished"
                    lastBest
            |]
    member this.result = _result