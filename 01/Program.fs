open Day01.Part1
open Day01.Part2

[<Literal>]
let inputPath = __SOURCE_DIRECTORY__ + "/input.txt"
let inputSeparator = "   "

[<EntryPoint>]
let main args =
    
    let part = args[0]
    
    match part with
    | "1" ->
        part1 inputPath inputSeparator
        0
    | "2" ->
        part2 inputPath inputSeparator
        0
    | _ ->
        printf "Unknown part"
        1
