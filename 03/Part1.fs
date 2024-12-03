namespace Day03

open System
open System.IO

module Part1 =
 
    type Mul = | Mul of int * int
    
    let mulRegex = System.Text.RegularExpressions.Regex("mul\(([0-9]{1,3}),([0-9]{1,3})\)")
    
    let mulInstructions str =
        let matches = mulRegex.Matches(str)
        
        matches
        |> Seq.map(fun m ->
            Mul(int m.Groups[1].Value, int m.Groups[2].Value)    
        )
        
    let executeInstructions (instructions: Mul seq) =
        instructions
        |> Seq.map(fun instr ->
            let (Mul (a, b)) = instr
            a * b
        )
        |> Seq.sum
    
    let part1 inputPath inputSeparator =
        let lines = File.ReadAllLines(inputPath)
        
        lines
        |> Array.map mulInstructions
        |> Seq.map executeInstructions
        |> Seq.sum
        |> printf "Total: %i"
        ()