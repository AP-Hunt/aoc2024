namespace Day09

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Text

module Part2 =
    
    type Block =
    | File of (int * int)   // length, id
    | Free of int           // length
    
    let parseBlockSequence (str: string)  : Block list =
        let nums = str.ToCharArray() |> Array.map (System.Char.GetNumericValue >> int) |> List.ofArray
        
        let toBlockI idx x =
            if idx % 2 = 0 then
                let fileId = idx / 2
                File(x, fileId)
            else
                Free(x)
                
        nums
        |> List.mapi toBlockI
        
    let formatBlockSequence (blocks: Block list) =
        let sb = StringBuilder()
        
        let formatBlock (sb: StringBuilder) block =
            match block with
            | File(len, i) -> Enumerable.Repeat(i.ToString(), len) |> String.Concat |> sb.Append
            | Free(len) -> Enumerable.Repeat(".", len) |> String.Concat |> sb.Append
        
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
            | Free _ -> 0
            | File(_, id) -> i * id
        )
        |> List.fold (fun acc x -> acc + (int64 x)) 0L
    
    let expandBlocks (blocks: Block list) =
        blocks
        |> List.map (function
            | Free(len) -> Enumerable.Repeat(Free(1), len).ToList() |> List.ofSeq
            | File(len, id) -> Enumerable.Repeat(File(1, id), len).ToList() |> List.ofSeq
        )
        |> List.collect id
    
    let largeEnoughFreeSpace (len: int) (block: Block) =
        match block with
        | File _ -> false
        | Free freeLen -> freeLen >= len
    
    let compact (blocks: Block list) =
        let mutBlocks = List(blocks)
        
        for i in (mutBlocks.Count - 1) .. -1 .. 0 do
            let blk = mutBlocks[i]
            
            match blk with
            | Free _ -> ()
            | File(fileLen, _) ->
                match mutBlocks.FindIndex (largeEnoughFreeSpace fileLen) with
                | -1 -> ()
                | freeSpaceIndex ->
                    if freeSpaceIndex < i then
                        let freeSpace = mutBlocks[freeSpaceIndex]
                        
                        match freeSpace with
                        | File _ -> ()
                        | Free freeLen ->
                            if freeLen = fileLen then
                                // Swap
                                mutBlocks[i] <- freeSpace
                                mutBlocks[freeSpaceIndex] <- blk
                            else
                                let remainder = freeLen - fileLen
                                mutBlocks[i] <- Free(fileLen)
                                mutBlocks[freeSpaceIndex] <- blk
                                mutBlocks.Insert(freeSpaceIndex + 1, Free(remainder))
                            
        mutBlocks |> List.ofSeq
        
    let part2 inputPath =
        let lines = parseBlockLines inputPath
        
        lines |> Array.iter (expandBlocks >> formatBlockSequence >> printfn "%s")
        
        printfn "Compacting lines ....."
        
        let compacted =
            lines |> Array.map compact
            
        compacted |> Array.iter (fun blocks ->
            let expanded = blocks |> expandBlocks
            expanded |> formatBlockSequence |> printf "%s"
            printfn $" ==> Checksum %i{checksum expanded}"
        )        
        
        ()