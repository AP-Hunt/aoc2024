namespace Day01

open System
open System.IO

module Part2 =
    
    let frequencyOf (map: Map<int, int>) (x: int) =
        map 
        |> Map.tryFind x
        |> Option.defaultValue 0
    
    let part2 inputPath (separator: string) =
        let inputLines = File.ReadAllLines inputPath

        let splitAsInt (line: string) =
            match line with
            | "" -> (0, 0)
            | _ ->
                let parts = line.Split(separator)
                let a = parts[0]
                let b = parts[1]
                
                (int a, int b)
        
        
        let (left, right) =
            inputLines
            |> Array.map splitAsInt
            |> Array.unzip
            
        let rightFrequency =
            right |> Array.countBy id |> Map.ofArray
        
        let frequencyOf' = frequencyOf rightFrequency
        
        let similarityScore (x: int) =
            x * (frequencyOf' x)
        
        left
        |> Array.map similarityScore
        |> Array.sum
        |> printfn "%i"