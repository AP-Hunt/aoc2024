namespace Day08

open System.Collections.Generic
open System.IO

module Part2 =
    
    type Entity =
    | Node of char
    | AntiNode of char

    type PuzzleMap = Entity list option array2d
        
    type MapBounds =
        {
            MinX: int
            MinY: int
            MaxX: int
            MaxY: int
        }
    
    let outOfBounds (bounds: MapBounds) (y, x) =
        (y < bounds.MinY) || (y > bounds.MaxY) || (x < bounds.MinX) || (x > bounds.MaxX)  
    
    let parseMapChars (chars: char array) =
        let toEntityOpt char =
            match char with
            | '.' -> None
            | c -> Some <| [Node c]
            
        chars |> Array.map toEntityOpt
    
    let parseMap inputPath : PuzzleMap =
        inputPath
        |> File.ReadAllLines
        |> Array.map _.ToCharArray()
        |> Array.map parseMapChars
        |> array2D
     
    let buildFrequencyNodeLookup (map: PuzzleMap) (bounds: MapBounds) =
        let lookup = Dictionary<char, (int * int) list>()
        
        for y = 0 to bounds.MaxY do
            for x = 0 to bounds.MaxX do
                let cell = Array2D.get map y x
                
                match cell with
                | None -> ()
                | Some(entities) ->
                    for ent in entities do
                        match ent with
                        | Node(c) ->
                            match lookup.TryGetValue(c) with
                            | true, lst ->
                                lookup[c] <- lst @ [(y, x)]
                            | false, _ -> lookup[c] <- [(y, x)]
                        | _ -> ()
        lookup
        
    let coordDistance (y1, x1) (y2, x2) = (y2 - y1, x2 - x1)
    let doubleCoord (y, x) = (y * 2, x * 2)
    let addCoord (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)
        
    let tryPlaceAntiNodeOnMap (map: PuzzleMap) (bounds: MapBounds) (y, x) antiNode =
        if outOfBounds bounds (y, x) then
            None 
        else 
            let cell = Array2D.get map y x
            match cell with
            | None ->
                Array2D.set map y x (Some <| [antiNode])
                Some(y, x)
            | Some(contents) ->
                Array2D.set map y x (Some <| (contents @ [antiNode]))
                Some (y, x)
        
    let projectAntiNodes map bounds start step antiNode =
        let rec projectRecursive map location step  =
            let antiNodeLocation = addCoord location step
            
            match tryPlaceAntiNodeOnMap map bounds antiNodeLocation antiNode with
            | None -> ()
            | Some(y', x') -> projectRecursive map (y', x') step
            
        projectRecursive map start step
        
    let part2 inputPath =
        let map = parseMap inputPath
        let bounds =
            {
                MapBounds.MinX = 0;
                MinY = 0;
                
                MaxX = (map |> Array2D.length2) - 1
                MaxY = (map |> Array2D.length1) - 1
            }
        
        let frequencyMap = buildFrequencyNodeLookup map bounds
        
        for entry in frequencyMap do
             let freq = entry.Key
             let nodes = entry.Value
             
             for n1 in nodes do
                 for n2 in nodes do
                     if n1 <> n2 then
                         let projectionStep = coordDistance n1 n2
                         
                         projectAntiNodes map bounds n1 projectionStep (AntiNode freq)
                     else
                         ()
        
        let uniqueAntiNodeLocations =
            let mutable n = 0
            map
            |> Array2D.iter (fun cell ->
                match cell with
                | None -> ()
                | Some(contents) ->
                    if contents |> List.exists _.IsAntiNode then
                        n <- n + 1
                    else ()
            )
            n
        
        printfn $"There are %i{uniqueAntiNodeLocations} unique locations"
        
        ()