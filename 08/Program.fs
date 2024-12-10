namespace Day08

module Main =
    [<Literal>]
    let inputPath = __SOURCE_DIRECTORY__ + "/input.txt"

    [<EntryPoint>]
    let main args =
        match args[0] with
        | "1" ->
            Part1.part1 inputPath
            0
        | "2" ->
            Part2.part2 inputPath
            0
        | _ ->
            printfn "Unknown part"
            1
