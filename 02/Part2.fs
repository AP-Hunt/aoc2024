namespace Day02

open System
open System.IO

module Part2 =
    
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
    
    let isValidTransition (direction: Direction) a b =
        match (a, b) with
        | SafeIncrease when direction.IsIncreasing -> true
        | SafeDecrease when direction.IsDecreasing -> true
        | _ -> false
    
            
    let reportDirection (levels: int array) =
        let tryDirectionOf (a: int, b: int) =
            match (a, b) with
                | SafeIncrease -> Some(Increasing)
                | SafeDecrease -> Some(Decreasing)
                | _ -> None
                
        levels |> Array.pairwise |> Array.tryPick tryDirectionOf
    
    let isValidReport (levels: int array) =
        let direction = reportDirection levels
        match direction with
        | None -> false
        | Some(direction) ->
            levels
            |> Array.pairwise
            |> Array.forall (fun (a, b) -> isValidTransition direction a b)
    
    let part2 inputPath inputSeparator =
        let levels = parseLevels inputPath inputSeparator      
                            
        let validWithTolerance (levels: int array) =
            if isValidReport levels then
                true
            else
                let reportsWithOneLevelRemoved =
                    seq {
                        for i in 0 .. ((Array.length levels) - 1) do levels |> Array.removeAt i
                    }
                
                reportsWithOneLevelRemoved
                |> Seq.exists isValidReport
                    
        
        levels
        |> Array.filter validWithTolerance
        |> Array.length
        |> printfn "Safe levels: %i"