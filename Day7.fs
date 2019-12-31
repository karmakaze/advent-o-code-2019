module Day7

open System.IO

let filename = "day7-input.txt"

let input =
    File.ReadLines(filename)
    |> Seq.head
    |> fun line -> line.Split ','
    |> Array.map int
    |> Array.toList

//let input = [3; 15; 3; 16; 1002; 16; 10; 16; 1; 16; 15; 15; 4; 15; 99; 0; 0]

type IntcodeComputer private () =
    [<DefaultValue>] val mutable readSource:List<int>
    [<DefaultValue>] val mutable writeSink:List<int>

    new(input:list<int>) as ic = IntcodeComputer()
                                 then ic.readSource <- input
                                      ic.writeSink <- []

    member ic.readInput =
        let value = ic.readSource.Head
        ic.readSource <- ic.readSource.Tail
        value

    member ic.writeOutput value =
        ic.writeSink <- value::ic.writeSink

    let mode (opcode:int) (index:int) =
        if index = 1 then
            (opcode / (100)) % 10
        elif index = 2 then
            (opcode / (1000)) % 10
        elif index = 3 then
            (opcode / (10000)) % 10
        else
            invalidArg "index" "must be 1, 2, or 3"

    let load (mode:int) (arg:int) (core:int[]) =
        if mode = 0 then
            core.[arg]
        elif mode = 1 then
            arg
        else invalidArg "mode" "mode be 0 or 1"

    let store (value:int) (mode:int) (arg:int) (core:int[]) =
        if mode = 0 then
            core.[arg] <- value
        elif mode = 1 then
            invalidArg "mode" "mode 1 not valid for store operand"
        else invalidArg "mode" "mode be 0 or 1"

    member ic.run pc (core:int[]) =
        let rec run pc (core:int[]) =
            let opcode = core.[pc]
            match opcode % 100 with
            | 1 -> // add
                let value1 = load (mode opcode 1) (core.[pc + 1]) core
                let value2 = load (mode opcode 2) (core.[pc + 2]) core
                let value = value1 + value2
                store value (mode opcode 3) (core.[pc + 3]) core
                run (pc + 4) core
            | 2 -> // multiply
                let value1 = load (mode opcode 1) (core.[pc + 1]) core
                let value2 = load (mode opcode 2) (core.[pc + 2]) core
                let value = value1 * value2
                store value (mode opcode 3) (core.[pc + 3]) core
                run (pc + 4) core
            | 3 -> // read input -> store at arg
                let value = ic.readInput
                store value (mode opcode 1) core.[pc + 1] core
                run (pc + 2) core
            | 4 -> // read from arg -> write output
                let value = load (mode opcode 1) (core.[pc + 1]) core
                ic.writeOutput value
                run (pc + 2) core
            | 5 -> // jump-if-true
                let value1 = load (mode opcode 1) (core.[pc + 1]) core
                if value1 <> 0 then
                    let value2 = load (mode opcode 2) (core.[pc + 2]) core
                    run value2 core
                else
                    run (pc + 3) core
            | 6 -> // jump-if-false
                let value1 = load (mode opcode 1) (core.[pc + 1]) core
                if value1 = 0 then
                    let value2 = load (mode opcode 2) (core.[pc + 2]) core
                    run value2 core
                else
                    run (pc + 3) core
            | 7 -> // less than
                let value1 = load (mode opcode 1) (core.[pc + 1]) core
                let value2 = load (mode opcode 2) (core.[pc + 2]) core
                let value =
                    if value1 < value2 then 1
                    else 0
                store value 0 (core.[pc + 3]) core
                run (pc + 4) core
            | 8 -> // equals
                let value1 = load (mode opcode 1) (core.[pc + 1]) core
                let value2 = load (mode opcode 2) (core.[pc + 2]) core
                let value =
                    if value1 = value2 then 1
                    else 0
                store value 0 (core.[pc + 3]) core
                run (pc + 4) core
            | 99 -> // halt
                ic.writeSink |> List.rev
            | _ -> // fail
                [ opcode ]
        run pc core

let runPhaseSetting [a; b; c; d; e] =
    let ampA = IntcodeComputer([a; 0])
    ampA.run 0 (input |> Array.ofList)
    let [outA] = ampA.writeSink

    let ampB = IntcodeComputer([b; outA])
    ampB.run 0 (input |> Array.ofList)
    let [outB] = ampB.writeSink

    let ampC = IntcodeComputer([c; outB])
    ampC.run 0 (input |> Array.ofList)
    let [outC] = ampC.writeSink

    let ampD = IntcodeComputer([d; outC])
    ampD.run 0 (input |> Array.ofList)
    let [outD] = ampD.writeSink

    let ampE = IntcodeComputer([e; outD])
    ampE.run 0 (input |> Array.ofList)
    let [outE] = ampE.writeSink

    outE

let allSettings = seq {
                        for a in 0 .. 4 do
                            for b in 0 .. 4 do
                                if b <> a then
                                    for c in 0 .. 4 do
                                        if c <> a && c <> b then
                                            for d in 0 .. 4 do
                                                if d <> a && d <> b && d <> c then
                                                    for e in 0 .. 4 do
                                                        if e <> a && e <> b && e <> c && e <> d then
                                                            [a; b; c; d; e]
                      }


let runAllSettings =
    allSettings
    |> Seq.map runPhaseSetting
    |> Seq.max

let Answer =
    runAllSettings
