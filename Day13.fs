module Day13

open System
open System.IO

let filename = "day13-input.txt"

let input =
    File.ReadLines(filename)
    |> Seq.head
    |> fun line -> line.Split ','
    |> Array.map System.Numerics.BigInteger.Parse
    |> Array.toList

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

type TileEnum = Empty=0 | Wall=1 | Block=2 | Paddle=3 | Ball=4

type Joy = Left = -1 | Centre = 0 | Right = 1

type PaintingRobot private () =
    [<DefaultValue>] val mutable grid:Map<int*int, TileEnum>

    new(dummy:int) as r =
        PaintingRobot()
        then r.grid <- Map.empty

    member r.PaintTile (x:int) (y:int) (tile:TileEnum) =
        r.grid <- r.grid.Add ((x, y), tile)

    member r.PaintedSquares =
        r.grid.Count

type IntcodeComputer private () =
    [<DefaultValue>] val mutable robot:PaintingRobot
    [<DefaultValue>] val mutable outX: int option
    [<DefaultValue>] val mutable outY: int option
    [<DefaultValue>] val mutable relativeBase:bigint
    [<DefaultValue>] val mutable lastBallPos:Option<int*int>
    [<DefaultValue>] val mutable currBallPos:Option<int*int>
    [<DefaultValue>] val mutable currPaddlePos:Option<int*int>
    [<DefaultValue>] val mutable score:bigint
    [<DefaultValue>] val mutable core:Map<int, bigint>

    new(robot:PaintingRobot, core:list<bigint>) as ic =
        IntcodeComputer()
        then ic.robot <- robot
             ic.outX <- None
             ic.outY <- None
             ic.lastBallPos <- None
             ic.currBallPos <- None
             ic.score <- bigint 0
             ic.core <- Map.empty
             for i in 0 .. (core.Length - 1) do
                 ic.core <- ic.core.Add (i, core.[i])

    member ic.readInput: bigint =
//        printfn "lastBall %A curBall %A paddle %A" ic.lastBallPos ic.currBallPos ic.currPaddlePos
        match (ic.currBallPos, ic.currPaddlePos) with
        | (Some ballPos, Some paddlePos) ->
            let (ballX, _) = ballPos
            let (paddleX, _) = paddlePos
            if paddleX < ballX then
                bigint 1
            elif paddleX > ballX then
                bigint -1
            else bigint 0
        | _ -> bigint -1

    member ic.writeOutput (value:bigint) =
        match (ic.outX, ic.outY) with
        | (None, _) ->
            ic.outX <- Some (int value)
            ()
        | (_, None) ->
            ic.outY <- Some (int value)
            ()
        | (Some x, Some y) ->
            ic.outX <- None
            ic.outY <- None
            if x = -1 && y = 0 then
                ic.score <- value
            else
                let tile = int value
                ic.robot.PaintTile x y (enum tile)
                match enum tile with
                | TileEnum.Paddle ->
                    ic.currPaddlePos <- Some (x, y)
                    ()
                | TileEnum.Ball ->
                    ic.lastBallPos <- ic.currBallPos
                    ic.currBallPos <- Some (x, y)
                    ()
                | _ -> ()
            ()

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
//            printfn "load mode %A addr %A" mode arg
            let addr = int arg
            match ic.core.TryFind (int arg) with
            | Some value -> value
            | None -> bigint 0
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
//            printfn "pc %A opcode %A (%A)" pc opcode (opcode % 100)
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
                []
            | _ -> // fail
                [ bigint opcode ]
        run pc

let Answer =
    let r = PaintingRobot(0)
    let ic = IntcodeComputer(r, input)
    ic.core <- ic.core.Add(0, bigint 2)
    ic.run 0 |> ignore
//    let blocks = r.grid |> Map.toList
//                        |> List.filter (fun (pos, tile) -> tile <> TileEnum.Empty && tile <> TileEnum.Wall)
//    blocks

//    let blank:char[] = Array.create (40 * 6) ' '
//    let raster = whiteSquares
//                 |> List.fold (fun (a:char[]) (x, y) -> a.[y * 40 + x] <- '#'; a) blank
//    for row in 0 .. 5 do
//        let line = (raster.[row * 40 .. row * 40 + 39] |> String.Concat)
//        printfn "%A" line
//    ()
    ic.score
