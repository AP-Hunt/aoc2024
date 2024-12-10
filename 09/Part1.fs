namespace Day09

open System
open System.Collections.Generic
open System.IO
open System.Text

module Part1 =
    
    type Block =
    | File of int
    | Free
    
    let parseBlockSequence (str: string)  : Block list =
        let nums = str.ToCharArray() |> Array.map (System.Char.GetNumericValue >> int) |> List.ofArray
        
        let toBlockI idx x =
            if idx % 2 = 0 then
                let fileId = idx / 2
                File(fileId) |> List.replicate x
            else
                Free |> List.replicate x
                
        nums
        |> List.mapi toBlockI
        |> List.collect id
        
    let formatBlockSequence (blocks: Block list) =
        let sb = StringBuilder()
        
        let formatBlock (sb: StringBuilder) block =
            match block with
            | File(i) -> sb.Append(i.ToString())
            | Free -> sb.Append(".")
        
        blocks
        |> List.fold formatBlock sb
        |> _.ToString()
        
    let parseBlockLines inputPath =
        inputPath
        |> File.ReadAllLines
        |> Array.map parseBlockSequence
    
    let checksum (blocks: Block list) =
        blocks
        |> List.mapi (fun i block ->
            match block with
            | Free -> 0
            | File(id) -> i * id
        )
        |> List.fold (fun acc x -> acc + (int64 x)) 0L
    
    let compact (blocks: Block list) =    
        let mutBlocks = List(blocks)
        
        let tryFindBack (lst: List<Block>) (fn: Block -> bool) (startingAt: int)=
            try
                let index = lst.FindLastIndex(startingAt, fn)
                if index = -1 then None else Some index
            with
            | :? ArgumentNullException -> None
        
        let mutable lastSwapIndex = mutBlocks.Count - 1
        for i = 0 to mutBlocks.Count - 1 do
            let b = mutBlocks[i]
            match b with
            | File _ -> ()
            | Free ->
                match tryFindBack mutBlocks _.IsFile lastSwapIndex with
                | None -> ()
                | Some(lastFileIdx) ->
                    if lastFileIdx > i then
                        lastSwapIndex <- lastFileIdx
                        let fileBlock = mutBlocks[lastFileIdx]
                        mutBlocks[i] <- fileBlock
                        mutBlocks[lastFileIdx] <- Free
        
        mutBlocks |> List.ofSeq
    
    let part1 inputPath =
        let lines = parseBlockLines inputPath
        
        lines |> Array.iter (formatBlockSequence >> printfn "%s")
        
        printfn "Compacting lines ....."
        
        let compacted =
            lines |> Array.map compact
            
        compacted |> Array.iter (fun blocks ->
            blocks |> formatBlockSequence |> printf "%s"
            printfn $" ==> Checksum %i{checksum blocks}"
        )        
        
        ()