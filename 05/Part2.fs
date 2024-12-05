namespace Day05

open System
open System.IO

module Part2 =
    
    // (Page number * Must come before pager number)
    type Rule = (int * int)
    
    let parseInput path =
        
        let lines = File.ReadAllLines path
        
        let separatorLineIndex = (lines |> Array.findIndex (fun l -> l = ""))
    
        let ruleLines = lines |> Array.take (separatorLineIndex)
        let updateLines = lines |> Array.skip (separatorLineIndex + 1)
        
        let rules: Rule array =
            ruleLines
            |> Array.map _.Split("|")
            |> Array.map (fun a -> (int a[0], int a[1]))
        
        let updates = updateLines |> Array.map (_.Split(",") >> Array.map int)
        
        (rules, updates)
    
    let isRuleObeyed  (pages: int array) (rule: (int * int )) =
        let (pageNum, otherNum) = rule
        
        let pageNumIdx = pages |> Array.tryFindIndex ((=) pageNum)
        let otherNumIdx = pages |> Array.tryFindIndex ((=) otherNum)
        
        match (pageNumIdx, otherNumIdx) with
        | (None, None) ->
            printfn "Rule %i|%i is obeyed because neither number appears" pageNum otherNum
            true
        | (None, Some(_)) ->
            printfn "Rule %i|%i is obeyed because only other num (%i) appears" pageNum otherNum otherNum
            true
        | (Some(_), None) ->
            printfn "Rule %i|%i is obeyed because only page num (%i) appears" pageNum otherNum pageNum
            true
        | (Some(pageNumIdx), Some(otherNumIdx)) ->
            if pageNumIdx < otherNumIdx then
                printfn "Rule %i|%i is obeyed because %i comes before %i" pageNum otherNum pageNum otherNum
                true
            else
                printfn "Rule %i|%i is NOT obeyed because %i comes after %i" pageNum otherNum pageNum otherNum
                false
    
    let relevantRules (rules: Rule array) (pageNum: int) =      
        rules
        |> Array.filter(fun (p, o) -> p = pageNum || o = pageNum)
    
    let sortRules (rules: Rule array) =
        let allNumbers =
            Array.append (rules |> Array.map fst) (rules |> Array.map snd)
            |> Array.distinct
      
        allNumbers
        |> Array.sortWith (fun a b ->     
            let aLessThanBRule = rules |> Array.tryFind (fun r -> r = (a, b))
            let bLessThanARule = rules |> Array.tryFind (fun r -> r = (b, a))
            
            match (aLessThanBRule, bLessThanARule) with
            | None, None -> 0
            | Some _, None -> -1
            | None, Some _ -> 1
            | Some _, Some _ -> raise (ArgumentException("both rules mention each other"))
        )
    
    let sortPagesByRules (pages: int array) (rules: Rule array) =
        pages
        |> Array.sortWith (fun a b ->     
            let aLessThanBRule = rules |> Array.tryFind (fun r -> r = (a, b))
            let bLessThanARule = rules |> Array.tryFind (fun r -> r = (b, a))
            
            match (aLessThanBRule, bLessThanARule) with
            | None, None -> 0
            | Some _, None -> -1
            | None, Some _ -> 1
            | Some _, Some _ -> raise (ArgumentException("both rules mention each other"))
        )
    
    let isUpdateValid (rules: Rule array) (pages: int array) =
        let result =
            rules
            |> Array.forall (isRuleObeyed pages)
            
        if result then
            printfn "VALID: %A" pages
        else
            printfn "INVALID: %A" pages
        
        result
    
    let part2 inputPath =
        let (rules, updates) = parseInput inputPath
        
        let middleDigit (nums: int array) =
            let mid =
                nums
                |> Array.length
                |> (fun x -> float x / 2.0)
                |> Math.Floor
                |> int
            
            nums[mid]
        
        let invalidUpdates =
            updates
            |> Array.filter (isUpdateValid rules >> not)
        
        let sortedUpdates =
            invalidUpdates
            |> Array.map (fun pages -> sortPagesByRules pages rules)
        
        printfn "SORTED UPDATES ======="
        
        if sortedUpdates |> Array.forall (isUpdateValid rules) then
            printfn "ALL SORTED UPDATES ARE VALID"
        else
            printfn "ONE OR MORE SORTED UPDATES AREN'T VALID"
        
        sortedUpdates
        |> Array.map middleDigit
        |> Array.sum
        |> printfn "Answer: %i"

