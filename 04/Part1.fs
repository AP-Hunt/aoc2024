namespace Day04

open System
open System.IO

module Part1 =

    let masks = [|
        // (y, x) pairs
        [| (0, 0); (0, -1); (0, -2); (0, -3); |]   // Left
        [| (0, 0); (0, 1); (0, 2); (0, 3); |]      // Right
        [| (0, 0); (-1, 0); (-2, 0); (-3, 0); |]   // Top
        [| (0, 0); (1, 0); (2, 0); (3, 0); |]      // Bottom
        
        [| (0, 0); (-1, -1); (-2, -2); (-3, -3); |] // Top Left
        [| (0, 0); (1, 1); (2, 2); (3, 3); |]       // Bottom right
        [| (0, 0); (-1, 1); (-2, 2); (-3, 3); |]    // Top right
        [| (0, 0); (1, -1); (2, -2); (3, -3); |]    // Bottom left
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
    
    let part1 (inputPath: string) =
        let ws = parseWordsearch inputPath
        
        let mutable count = 0
        let target = "XMAS"
        
        for y in 0 .. Array2D.length1 ws - 1 do
            for x in 0 .. Array2D.length2 ws - 1 do
                let c = Array2D.get ws y x
                match c with
                | 'X' ->
                    let words =
                        masks
                        |> Array.map (applyMask ws (x, y))
                    
                    let xmasWords = words |> Array.filter ((=) target)
                        
                    count <- count + (xmasWords |> Array.length)
                    ()
                | _ -> ()
                
        printfn "There are %i XMAS entries" count
        ()

