namespace Day11

open System
open System.IO
open System.Linq
open FSharp.Collections.ParallelSeq
open System.Collections.Generic


module Part2 =
    
    let parseStones inputPath =
        inputPath |> File.ReadAllText |> _.Split(" ") |> Array.map _.Trim() |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Seq.ofArray
    
    let (|Zero|EvenDigits|MultBy2024|) stone =
        if stone = "0" then
            Zero
        else if stone.Length % 2 = 0 then
            EvenDigits
        else
            MultBy2024
            
    let expandStone (stone: string)=
        match stone with
        | Zero -> [|"1"|]
        | EvenDigits ->
            let len = stone.Length
            let sanitise (str: string) =
                let trimmed = str.TrimStart('0')
                
                if trimmed.Length = 0 then "0" else trimmed
            [|stone.Substring(0, len/2) |> sanitise; stone.Substring(len/2) |> sanitise |]
        | MultBy2024 ->
            let n = (stone |> int64) * 2024L
            [|n.ToString()|]            
            
    let expandStones (stones: List<string>) =   
        let mutable i = 0
        while i < stones.Count do
            let stone = stones[i]
            
            match stone with
            | Zero ->
                stones.RemoveAt(i)
                stones.Insert(i, "1")
                stones[i] <- "1"
                i <- i + 1
            | MultBy2024 ->
                let n = (stone |> int64) * 2024L
                stones.RemoveAt(i)
                stones.Insert(i, string n)
                stones[i] <- string n
                i <- i + 1
            | EvenDigits ->
                let len = stone.Length
                let sanitise (str: string) =
                    let trimmed = str.TrimStart('0')
                    
                    if trimmed.Length = 0 then "0" else trimmed
                   
                let left = stone.Substring(0, len/2) |> sanitise
                let right = stone.Substring(len/2) |> sanitise
                
                stones.RemoveAt(i)
                stones.Add(left)
                stones.Add(right)
                i <- i + 1
        stones
        
        //stones
        //|> Array.collect expandStone
    
    let expandStoneNTimes n (stone: string) =
        let mutable stones = List<string>()
        stones.Add(stone)
        
        for i in 1 .. n do
            expandStones stones |> ignore
            printfn $"Expanded stone %s{stone} by %i{i} steps"
            
        stones
        
    let expandCountNTimes (n: int) (stones: string seq)=
        let lookup = Dictionary<string, int64>()
        
        for s in stones do
            lookup.Add(s, 1)
        
        let incrementLookupCounter (lookup: Dictionary<string, int64>) key n = 
            match lookup.TryGetValue(key) with
            | true, count ->
                lookup[key] <- count + n
            | false, _ ->
                lookup[key] <- n
                
        let decrementLookupCounter (lookup: Dictionary<string, int64>) key n = 
            match lookup.TryGetValue(key) with
            | true, count ->
                lookup[key] <- count - n
            | false, _ ->
                lookup[key] <- 0          

        for i in 1 .. n do
            for kvp in lookup.Where(fun kvp -> kvp.Value > 0).ToList() do
                let stone = kvp.Key
                let count = kvp.Value
                
                match stone with
                | Zero ->
                    incrementLookupCounter lookup "1" count
                    decrementLookupCounter lookup stone count
                | MultBy2024 ->
                    let mult = string <| (int64 stone) * 2024L
                    incrementLookupCounter lookup mult count
                    decrementLookupCounter lookup stone count
                | EvenDigits ->
                    let len = stone.Length
                    let sanitise (str: string) =
                        let trimmed = str.TrimStart('0')
                        
                        if trimmed.Length = 0 then "0" else trimmed
                       
                    let left = stone.Substring(0, len/2) |> sanitise
                    let right = stone.Substring(len/2) |> sanitise
                    
                    incrementLookupCounter lookup left count
                    incrementLookupCounter lookup right count
                    decrementLookupCounter lookup stone count
   
        lookup.Values.Sum()
        
    let part2 inputPath =
        let stones = parseStones inputPath
        
        printfn $"Initial %A{stones}"

        let result = expandCountNTimes 75 stones
            
        printfn $"There are %i{result} stones"
        ()