namespace Day07

open System
open System.IO
open System.Text

module Part1 =
    
    type Line = (int64 * int array)
    
    type Operator =
    | Add
    | Multiply
    
    type Operation = (int * Operator)
    
    let parseLine (str: string) =
        let parts = str.Split(":")
        let target = parts[0] |> int64
        let numbers = parts[1].Split(" ") |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Array.map (_.Trim() >> int)
        (target, numbers)
    
    let parseInput inputPath: Line array =
        inputPath
        |> File.ReadAllLines
        |> Array.map parseLine
        
    let permutationsOf(numbers: int array) =
        
        let rec permute (numbers: int list) (acc:  (Operation list) list) =
            match numbers with
            | h :: t ->
                
                match acc with
                | [] -> 
                    permute t [ [(h, Multiply)]; [(h, Add)] ]
                | acc ->
                    let accWithMult = acc |> List.map ( fun ops -> ops @ [ (h, Multiply) ])
                    let accWithAdd = acc |> List.map ( fun ops -> ops @ [ (h, Add) ])
                    
                    let acc' = accWithMult @ accWithAdd
                    
                    permute t acc'
            | [] -> acc
        
        let requiredLength = (numbers |> Array.length) - 1 
        let startOp = (numbers[0], Add)
        
        permute (numbers |> Array.skip 1 |> List.ofArray) []
        |> List.filter (fun l -> List.length l = requiredLength)
        |> List.map (fun l -> [startOp] @ l)
        
    let executeOperators (ops: Operation list) =
        let applyOperator (acc: int64) (n: int, op: Operator) =
            match op with
            | Add -> acc + (int64 n)
            | Multiply -> acc * (int64 n)
            
        ops |> List.fold applyOperator 0L
        
    let formatOperations (ops: Operation list) =
        let sb = StringBuilder()
        let folder (sb: StringBuilder) (n: int, op: Operator) =
            match op with
            | Add -> sb.Append($"+%i{n}")
            | Multiply -> sb.Append($"*%i{n}")
            
        ops |> List.fold folder sb |> _.ToString()
        
        
    let lineIsValid (target: int64, numbers: int array) =
        printfn $"Target: %i{target}, Numbers: %A{numbers}"
        
        permutationsOf numbers
        |> List.exists (fun ops ->
            if executeOperators ops = target then
                let opsStr = formatOperations ops
                printfn $"-- %i{target} = %s{opsStr}"
                true
            else
                false
        )
        
    let part1 inputPath =
        let lines = parseInput inputPath
        
        let validLines = lines |> Array.filter lineIsValid
        
        let sum = validLines |> Array.sumBy fst
        
        printfn "================"
        printfn $"The answer is %i{sum}"
        ()