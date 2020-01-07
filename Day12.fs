module Day12

open System
open System.IO
open System.Text.RegularExpressions

let filename = "day12-input.txt"

let input =
    File.ReadLines(filename)

//let input = seq {
//                yield "<x=-1, y=0, z=2>"
//                yield "<x=2, y=-10, z=-7>"
//                yield "<x=4, y=-8, z=8>"
//                yield "<x=3, y=5, z=-1>"
//                }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    match m.Success with
    | true -> Some(List.tail [for g in m.Groups -> g.Value])
    | _    -> None

let inputPositions =
    input
    |> Seq.toList
    |> List.map (fun line ->
                     match line with
                     | Regex @"<x=(-?[0-9]+), y=(-?[0-9]+), z=(-?[0-9]+)>" [x; y; z] -> (int x, int y, int z)
                     | _ -> invalidArg "xyz" "failed to parse form")

type Body private () =
    [<DefaultValue>] val mutable position: int*int*int
    [<DefaultValue>] val mutable velocity: int*int*int
    [<DefaultValue>] val mutable gravities: List<int*int*int>

    new(x, y, z) as b =
        Body()
        then
            b.position <- (x, y, z)
            b.velocity <- (0, 0, 0)
            b.gravities <- []

    member b.gravity (m:Body) =
        let (bx, by, bz) = b.position
        let (mx, my, mz) = m.position
        (sign (mx - bx), sign (my - by), sign (mz - bz))

    member b.addGravity g =
        b.gravities <- g :: b.gravities

    member b.subGravity g =
        let (gx, gy, gz) = g
        b.gravities <- (-gx, -gy, -gz) :: b.gravities

    let vecToList (x:int, y:int, z:int) =
        [x; y; z]

    let sum (a, b, c) (x, y, z) =
        (a + x, b + y, c + z)

    let sumMag vec =
        vec
        |> vecToList
        |> List.map (fun x -> abs x)
        |> Seq.fold (fun a x -> a + x) 0

    member b.gravitate =
        b.gravities |> List.iter (fun g -> b.velocity <- sum b.velocity g)
        b.gravities <- []
        b.position <- sum b.position b.velocity

    member b.potentialEnergy =
        sumMag b.position

    member b.kineticEnergy =
        sumMag b.velocity

    member b.energy =
        b.potentialEnergy * b.kineticEnergy

type Simulator private () =
    [<DefaultValue>] val mutable bodies:List<Body>

    new(bodies) as s =
        Simulator()
        then
            s.bodies <- bodies

    member s.step =
        for i in 0 .. (s.bodies.Length - 1) do
            let a = s.bodies.[i]
            for b in s.bodies.[(i + 1) ..] do
                let g = a.gravity b
                a.addGravity g
                b.subGravity g
        for b in s.bodies do
            b.gravitate

    member s.run (count:int) =
        let rec run (count:int) =
            if count <= 0 then
                0
            else
                s.step
                run (count - 1)
        run count

    member s.totalEnergy =
        let energies = s.bodies |> List.map (fun b -> b.energy)
        energies |> List.fold (fun a x -> a + x) 0

    member s.print =
        for body in s.bodies do
            printfn "Pos %A Vel %A Nrg %A" body.position body.velocity body.energy

let Answer =
    let bodies = inputPositions |> List.map (fun (x, y, z) -> Body((x, y, z)))
    let sim = Simulator(bodies)
    sim.run 1000 |> ignore
    sim.print
    sim.totalEnergy
