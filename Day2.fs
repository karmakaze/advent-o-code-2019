module Day2

open System.IO

    let filename = "day2-input.txt"

    let input =
        File.ReadLines(filename)
        |> Seq.head
        |> fun x -> x.Split ','
        |> Array.map int

    let inputCopy =
        input |> Array.copy

    // part 1

    let state1202 (core:int[]) =
        core.[1] <- 12
        core.[2] <- 2
        core

    let rec run pc (core:int[]) =
        let opcode = core.[pc]
        if opcode = 1 then
            core.[core.[pc + 3]] <- core.[core.[pc + 1]] + core.[core.[pc + 2]]
            run (pc + 4) core
        elif opcode = 2 then
            core.[core.[pc + 3]] <- core.[core.[pc + 1]] * core.[core.[pc + 2]]
            run (pc + 4) core
        elif opcode = 99 then
            core
        else [| -1 |]

    let Run core = run 0 core

    let Answer1 = (Run (state1202 (input |> Array.copy))).[0]

    // part 2

    let restate (noun, verb) (core:int[]) =
        core.[1] <- noun
        core.[2] <- verb
        core

    let allNounVerbs =
        seq {
            for noun in 0 .. 99 do
                for verb in 0 .. 99 ->
                    (noun, verb)
            }

    let findNounVerb value =
        allNounVerbs
        |> Seq.map (fun (noun, verb) ->
                        let core = restate (noun, verb) (input |> Array.copy)
                        (Run core, (noun, verb)))
        |> Seq.find (fun (core, _) -> core.[0] = value)
        |> fun (_, nounVerb) -> nounVerb

    let Answer2 = findNounVerb 19690720
