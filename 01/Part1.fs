namespace Day01

open System
open System.IO

module Part1 =
    
    let part1 inputPath (separator: string) =
        let inputLines = File.ReadAllLines inputPath

        let splitAsInt (line: string) =
            match line with
            | "" -> (0, 0)
            | _ ->
                let parts = line.Split(separator)
                let a = parts[0]
                let b = parts[1]
                
                (int a, int b)

        let sort2 arrayA arrayB =
            (
                Array.sort arrayA,
                Array.sort arrayB
            )

        let distance (a: int) (b: int) = Math.Abs(a-b)

        inputLines
        |> Array.map splitAsInt
        |> Array.unzip
        ||> sort2
        ||> Array.map2 distance
        |> Array.sum
        |> printfn "%i"