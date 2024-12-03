namespace Day03

open System
open System.IO
open System.Text.RegularExpressions

module Part2 =

    type Instruction =
    | Mul of int * int
    | Do
    | Dont
    
    type InstructionExecutionState =
        {
            Enabled: bool
            Total: int64
        }
    
    let mulRegex = System.Text.RegularExpressions.Regex("mul\(([0-9]{1,3}),([0-9]{1,3})\)|do\(\)|don't\(\)")
    
    let toInstruction (regexMatch: Match) =
        let fullStr = regexMatch.Groups[0].Value 
        if fullStr.StartsWith("don't") then
            Dont
        else if fullStr.StartsWith("do") then
            Do
        else if fullStr.StartsWith("mul") then
            Mul(int regexMatch.Groups[1].Value, int regexMatch.Groups[2].Value)
        else
            raise (ArgumentException("unexpected match"))
                
    
    let findInstructions str =
        let matches = mulRegex.Matches(str)
        
        matches
        |> Seq.map toInstruction
        
    let executeInstructions (state: InstructionExecutionState) (instruction: Instruction) =
        match instruction with
        | Do ->
            { state with Enabled = true }
        | Dont ->
            { state with Enabled = false }
        | Mul(a, b) ->
            if state.Enabled then
                { state with Total = state.Total + (int64 a * int64 b) }
            else
                state
    
    let part2 inputPath inputSeparator =
        let lines = File.ReadAllLines(inputPath)
       
        lines
        |> Array.map findInstructions
        |> Array.reduce Seq.append
        |> Seq.fold executeInstructions {Enabled = true; Total = 0; }
        |> _.Total
        |> printf "Total: %i"
        ()