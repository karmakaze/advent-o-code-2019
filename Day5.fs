module Day5

open System.IO

let filename = "day5-input.txt"

let input =
    File.ReadLines(filename)
    |> Seq.head
    |> fun line -> line.Split ','
    |> Array.map int

//let input = [| 3; 0; 4; 0; 99 |]
//let input = [| 1002; 4; 3; 4; 33 |]

let mutable readSource = [ 1 ]
let mutable (writeSink:List<int>) = []

let readInput =
    let value = readSource.Head
    readSource <- readSource.Tail
    value

let writeOutput value =
    writeSink <- value::writeSink

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
        let value = readInput
        store value (mode opcode 1) core.[pc + 1] core
        run (pc + 2) core
    | 4 -> // read from arg -> write output
        let value = load (mode opcode 1) (core.[pc + 1]) core
        writeOutput value
        run (pc + 2) core
    | 99 -> // halt
        writeSink |> List.rev
    | _ -> // fail
        [ opcode ]

let Answer = run 0 input
