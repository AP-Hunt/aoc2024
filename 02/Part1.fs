namespace Day02

open System
open System.IO

module Part1 =
    
    type Direction =
    | Increasing
    | Decreasing
    
    let parseLevels inputPath (separator: string) =
        
        inputPath
        |> File.ReadAllLines
        |> Array.map (_.Split(separator) >> Array.map int)
    
    let (|NoChange|SafeDecrease|SafeIncrease|UnsafeDecrease|UnsafeIncrease|) (a: int, b: int) =    
        match Math.Abs(a-b) with
        | 0 -> NoChange      
        | 1 | 2 | 3 when a > b -> SafeDecrease
        | 1 | 2 | 3 when a < b -> SafeIncrease
        | _ when a > b -> UnsafeDecrease
        | _ when a < b -> UnsafeIncrease
        | i -> raise (ArgumentException(sprintf "unaccounted for value %i" i))
    
    let part1 inputPath inputSeparator =
        let levels = parseLevels inputPath inputSeparator
        
        let numInvalidTransitions (level: int array): int =
            let pairs = level |> Array.pairwise
            
            let tryDirectionOf (a: int, b: int) =
                match (a, b) with
                | SafeIncrease -> Some(Increasing)
                | SafeDecrease -> Some(Decreasing)
                | _ -> None
            
            let levelDirection = pairs |> Array.tryPick tryDirectionOf

            match levelDirection with
            | None -> Int32.MaxValue 
            | Some(direction) ->
                pairs
                |> Array.map (fun (a: int, b: int) ->
                    match (a, b) with
                    | SafeIncrease when direction.IsIncreasing -> 0
                    | SafeDecrease when direction.IsDecreasing -> 0
                    | _ -> 1
                )
                |> Array.sum
                

        levels
        |> Array.map numInvalidTransitions
        |> Array.filter (fun x -> x = 0)
        |> Array.length
        |> printfn "Safe levels: %i"