﻿module Day6

open System.IO

let filename = "day6-input.txt"

let input = File.ReadLines(filename)

//let input = seq {
//            "COM)B"
//            "B)C"
//            "C)D"
//            "D)E"
//            "E)F"
//            "B)G"
//            "G)H"
//            "D)I"
//            "E)J"
//            "J)K"
//            "K)L"
//            "K)YOU"
//            "I)SAN"
//            }

let inputList = input |>Seq.toList

let orbits =
    inputList
    |> List.map (fun s -> s.Split ')')
    |> List.map (fun arr -> (arr.[0], arr.[1]))

type Vertex = string

type TreeGraph =
    val mutable roots: Set<Vertex>
    val mutable vertices: Set<Vertex>
    val mutable inEdges: Map<Vertex, Set<Vertex>>
    val mutable outEdges: Map<Vertex, Set<Vertex>>

    new() = {
            roots = Set.empty
            vertices = Set.empty
            inEdges = Map.empty
            outEdges = Map.empty
        }

    member g.comings (vertex:Vertex) =
        match g.inEdges.TryFind vertex with
        | Some s -> s
        | None -> Set.empty

    member g.goings (vertex:Vertex) =
        match g.outEdges.TryFind vertex with
        | Some s -> s
        | None -> Set.empty

    member g.addOrbit (center:Vertex, satellite:Vertex) =
        if not (g.vertices.Contains center) then
            g.roots <- g.roots.Add center
        g.vertices <- g.vertices.Add center

        if g.roots.Contains satellite then
            g.roots <- g.roots.Remove satellite
        else g.vertices <- g.vertices.Add satellite

        let goings = g.goings center
        g.outEdges <- g.outEdges.Add (center, (goings.Add satellite))

        let comings = g.comings satellite
        g.inEdges <- g.inEdges.Add (satellite, (comings.Add center))

    member g.countOrbits =
        let rec countOrbits (level:int) (vertex:Vertex) =
            (level+1) + (g.goings vertex
                         |> Set.fold (fun total going -> total + countOrbits (level + 1) going) 0)

        g.roots |> Set.fold (fun total root -> countOrbits -1 root) 0

    member g.pathToRoot (vertex:Vertex) =
        let rec pathToRoot path (vertex:Vertex) =
            let comings = g.comings vertex
            if comings.Count = 1 then
                comings |> Set.fold (fun path coming -> pathToRoot (coming::path) coming) path
            elif vertex = "COM" then
                path
            else
                printfn "path %A vertex %A comings %A" path vertex comings
                []
        pathToRoot [] vertex

    member g.printTree (indent:string) (vertex:Vertex) : unit =
        printfn "%s%A" indent vertex
        g.outEdges.TryFind vertex
        |> Option.map (fun outs -> outs |> Set.map (fun out -> g.printTree (indent+"| ") out)) |> ignore
        ()

let Answer =
    let g = TreeGraph()
    orbits |> List.map g.addOrbit |> ignore
    let rootToYou = g.pathToRoot "YOU"
    let rootToSan = g.pathToRoot "SAN"
    let paths = Seq.zip (rootToYou |> List.toSeq) (rootToSan |> List.toSeq)
    let commonPath = paths |> Seq.takeWhile (fun (y, s) -> y = s) |> Seq.toList
    let (_, uniqueToYou) = rootToYou |> (List.splitAt commonPath.Length)
    let (_, uniqueToSan) = rootToSan |> (List.splitAt commonPath.Length)
    uniqueToYou.Length + uniqueToSan.Length
