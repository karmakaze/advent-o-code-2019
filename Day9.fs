module Day9

open System
open System.IO

let filename = "day9-input.txt"

let input =
    File.ReadLines(filename)
    |> Seq.head
    |> fun line -> line.Split ','
    |> Array.map int
    |> Array.toList
    |> List.map (fun i -> bigint i)

//let input = [109; 1; 204; -1; 1001; 100; 1; 100; 1008; 100; 16; 101; 1006;  101; 0; 99]
//            |> List.map (fun i -> bigint i)

//let input = [1102; 34915192; 34915192; 7; 4; 7; 99; 0]
//            |> List.map (fun i -> bigint i)
//let input = [bigint 104; bigint 1125899906842624L; bigint 99]

type Core private () =
    [<DefaultValue>] val mutable memory:Map<int, bigint>

    new(initial:list<bigint>) as c = Core()
                                     then initial |> List.fold (fun i v -> (c.Set i v); i + 1) 0 |> ignore

    member c.Set (address:int) (value:bigint) =
        c.memory <- c.memory.Add (address, value)

type IntcodeComputer private () =
    [<DefaultValue>] val mutable readSource:List<bigint>
    [<DefaultValue>] val mutable writeSink:List<bigint>
    [<DefaultValue>] val mutable relativeBase:bigint
    [<DefaultValue>] val mutable core:Map<int, bigint>

    new(input:list<bigint>, core:list<bigint>) as ic = IntcodeComputer()
                                                       then ic.readSource <- input
                                                            ic.writeSink <- []
                                                            ic.core <- Map.empty
                                                            for i in 0 .. (core.Length - 1) do
                                                                ic.core <- ic.core.Add (i, core.[i])

    member ic.readInput =
        let value = ic.readSource.Head
        ic.readSource <- ic.readSource.Tail
        value

    member ic.writeOutput value =
        ic.writeSink <- value::ic.writeSink

    let mode (opcode:int) (index:int) =
        if index = 1 then
            (opcode / 100) % 10
        elif index = 2 then
            (opcode / 1000) % 10
        elif index = 3 then
            (opcode / 10000) % 10
        else
            invalidArg "index" "must be 1, 2, or 3"

    member ic.load (mode:int) (arg:bigint) =
        if mode = 0 then
            ic.core.[int arg]
        elif mode = 1 then
            arg
        elif mode = 2 then
            ic.core.[int (ic.relativeBase + arg)]
        else invalidArg "mode" "mode be 0, 1, or 2"

    member ic.store (value:bigint) (mode:int) (arg:bigint) =
        if mode = 0 then
            ic.core <- ic.core.Add (int arg, value)
        elif mode = 1 then
            invalidArg "mode" "mode 1 not valid for store operand"
        elif mode = 2 then
            ic.core <- ic.core.Add (int (ic.relativeBase + arg), value)
        else invalidArg "mode" "mode be 0, 1, or 2"

    member ic.run pc =
        let rec run pc =
            let opcode = int (ic.core.[pc])
            match (opcode % 100) with
            | 1 -> // add
                let value1 = ic.load (mode opcode 1) (ic.core.[pc + 1])
                let value2 = ic.load (mode opcode 2) (ic.core.[pc + 2])
                let value = value1 + value2
                ic.store value (mode opcode 3) (ic.core.[pc + 3])
                run (pc + 4)
            | 2 -> // multiply
                let value1 = ic.load (mode opcode 1) (ic.core.[pc + 1])
                let value2 = ic.load (mode opcode 2) (ic.core.[pc + 2])
                let value = value1 * value2
                ic.store value (mode opcode 3) (ic.core.[pc + 3])
                run (pc + 4)
            | 3 -> // read input -> store at arg
                let value = ic.readInput
                ic.store value (mode opcode 1) ic.core.[pc + 1]
                run (pc + 2)
            | 4 -> // read from arg -> write output
                let value = ic.load (mode opcode 1) (ic.core.[pc + 1])
                ic.writeOutput value
                run (pc + 2)
            | 5 -> // jump-if-true
                let value1 = ic.load (mode opcode 1) (ic.core.[pc + 1])
                if value1 <> (bigint 0) then
                    let value2 = ic.load (mode opcode 2) (ic.core.[pc + 2])
                    run (int value2)
                else
                    run (pc + 3)
            | 6 -> // jump-if-false
                let value1 = ic.load (mode opcode 1) (ic.core.[pc + 1])
                if value1 = (bigint 0) then
                    let value2 = ic.load (mode opcode 2) (ic.core.[pc + 2])
                    run (int value2)
                else
                    run (pc + 3)
            | 7 -> // less than
                let value1 = ic.load (mode opcode 1) (ic.core.[pc + 1])
                let value2 = ic.load (mode opcode 2) (ic.core.[pc + 2])
                let value =
                    if value1 < value2 then bigint 1
                    else bigint 0
                ic.store value (mode opcode 3) (ic.core.[pc + 3])
                run (pc + 4)
            | 8 -> // equals
                let value1 = ic.load (mode opcode 1) (ic.core.[pc + 1])
                let value2 = ic.load (mode opcode 2) (ic.core.[pc + 2])
                let value =
                    if value1 = value2 then bigint 1
                    else bigint 0
                ic.store value (mode opcode 3) (ic.core.[pc + 3])
                run (pc + 4)
            | 9 -> // adjust relativeBase
                let value = ic.load (mode opcode 1) (ic.core.[pc + 1])
                ic.relativeBase <- ic.relativeBase + value
                run (pc + 2)
            | 99 -> // halt
                ic.writeSink |> List.rev
            | _ -> // fail
                [ bigint opcode ]
        run pc

let Answer =
    let ic = IntcodeComputer([bigint 1], input)
    ic.run 0
    ic.writeSink
