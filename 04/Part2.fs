namespace Day04

open System
open System.IO

module Part2 =
    
    let masks = [|
        // (y, x) pairs
        [| (-1, -1); (0, 0); (1, 1); |]   // Top left to bottom right
        [| (1, -1); (0, 0); (-1, 1); |]   // Bottom left to top right
        
    |]
    
    let parseWordsearch (inputPath: string) =
        
        inputPath
        |> File.ReadAllLines
        |> Array.map _.ToCharArray()
        |> array2D
    
    let wordSearchChars (ws: char array2d) (coords: (int * int) array) =
        let maxY = (Array2D.length1 ws) - 1
        let maxX = (Array2D.length2 ws) - 1
        
        let getCharFromWs (x: int, y: int) =
            if (x > maxX || y > maxY) || (x < 0 || y < 0) then
                ' '
            else
                Array2D.get ws y x

        coords
        |> Array.map getCharFromWs
        
    
    
    let applyMask (ws: char array2d) (x: int, y: int) (mask: (int *int) array) =
        mask
        |> Array.map (fun (y', x') -> (x+x', y+y'))
        |> wordSearchChars ws
        |> String
    
    let part2 (inputPath: string) =
        let ws = parseWordsearch inputPath
        
        let mutable count = 0
        
        let isSAMorMAS str =
            match str with
            | "SAM" | "MAS" -> true
            | _ -> false
        
        for y in 0 .. Array2D.length1 ws - 1 do
            for x in 0 .. Array2D.length2 ws - 1 do
                let c = Array2D.get ws y x
                match c with
                | 'A' ->
                    let words =
                        masks
                        |> Array.map (applyMask ws (x, y))
                    
                    let isAnX = if words |> Array.forall isSAMorMAS then 1 else 0
                        
                    count <- count + isAnX
                    ()
                | _ -> ()
                
        printfn "There are %i XMAS entries" count
        ()
