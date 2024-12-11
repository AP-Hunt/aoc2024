namespace Day11

open System
open System.IO

module Part1 =
    
    let parseStones inputPath =
        inputPath |> File.ReadAllText |> _.Split(" ") |> Array.map _.Trim() |> Array.filter (String.IsNullOrWhiteSpace >> not)
    
    let (|Zero|EvenDigits|MultBy2024|) stone =
        if stone = "0" then
            Zero
        else if stone.Length % 2 = 0 then
            EvenDigits
        else
            MultBy2024
            
    let expandStones (stones: string array) =
        let expandStone (stone: string) =
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
                [|n |> string|]
                
        stones
        |> Array.collect expandStone
    
    let part1 inputPath =
        let stones = parseStones inputPath
        
        let mutable result = stones
        printfn $"Initial %A{result}"
        for i in 1 .. 25 do
            result <- expandStones result
            printfn $"Step %i{i} has %i{result.Length} stones"
        
        printfn $"There are %i{result.Length} stones"
        ()