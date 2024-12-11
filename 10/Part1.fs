namespace Day10

open System
open System.IO

module Part1 =
    type PathElement = int * int * int
    type Path = PathElement list
    
    let parseMap inputPath =
        inputPath
        |> File.ReadAllLines
        |> Array.map _.ToCharArray()
        |> Array.map (Array.map (Char.GetNumericValue >> int))
        |> array2D
    
    let findPaths (map: int array2d) startPoint =
       
        let hasReachedDestination (path: Path) =
            let (_, _, value) = List.last path
            value = 9
            
        let inBounds (map: int array2d) (y, x) =
            let maxY = Array2D.length1 map - 1
            let maxX = Array2D.length2 map - 1
            
            not <| ((y < 0 ) || (y > maxY) || (x < 0) || (x > maxX))
        
        let findValidNextSteps (map: int array2d) (y: int, x: int, value: int): PathElement list =
            [
                (y-1, x);
                (y+1, x);
                (y, x-1);
                (y, x+1);
            ]
            |> List.filter (inBounds map)
            |> List.map (fun (y, x) ->
                let v = Array2D.get map y x
                (y, x, v)    
            )
            |> List.filter (fun (_, _, value') ->
                value' - value = 1
            )
        
        let pathAppend path element = path @ [element]
        
        let rec stepPaths (map: int array2d) (pathsToStep: Path list) (steppedPaths: Path list) =
            match pathsToStep with
            | [] -> steppedPaths
            | h :: t ->
                let last = List.last h
                let validSteps = findValidNextSteps map last
                
                printfn $"From %A{last} there are %i{validSteps.Length} valid steps: %A{validSteps}"
                
                let steppedPaths' =
                    let newPaths = validSteps |> List.map (pathAppend h)
                    steppedPaths @ newPaths
                
                stepPaths map t steppedPaths'
        
        let rec expandRecursive (map: int array2d) (paths: Path list) =
            if paths.IsEmpty then
                []
            else if paths |> List.forall hasReachedDestination then
                paths
            else
                expandRecursive map (stepPaths map paths [])
        
        let y, x = startPoint
        expandRecursive map [ [(y, x, 0)] ]
    
    let part1 inputPath =
        let map = parseMap inputPath
        
        printfn "MAP"
        printfn $"%A{map}"
        
        let scoredMap = map |> Array2D.mapi (fun y x v ->
            if v <> 0 then
                0
            else
                findPaths map (y, x)
                |> List.countBy (fun path ->
                    let (y, x, _) = List.last path
                    (y, x)
                )
                |> List.length
        )
        printfn ""
        
        printfn "SCORED MAP"
        printfn $"%A{scoredMap}"
        printfn ""
        
        let mutable mapSum = 0
        scoredMap |> Array2D.iter (fun n -> mapSum <- mapSum + n)
        printfn $"The score is %i{mapSum}"
        ()