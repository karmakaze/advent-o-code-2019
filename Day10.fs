module Day10

open System
open System.IO

let filename = "day10-input.txt"

let input =
    File.ReadLines(filename)

//let input = seq {
//                ".#..#"
//                "....."
//                "#####"
//                "....#"
//                "...##"
//                }

let coOrds (input: string seq): List<int*int> =
    input
    |> Seq.mapi (fun i line -> line |> Seq.mapi (fun j ch -> (i, j, ch))
                                    |> Seq.filter (fun (i, j, ch) -> ch <> '.')
                                    |> Seq.map (fun (i, j, ch) -> (j, i)))
    |> Seq.concat
    |> Seq.toList

let rec gcd (a:int) (b:int) =
    if a = 0 then b
    elif b = 0 then a
    else
        let q = a / b
        let r = a % b
        gcd b r

let mag (mx, my) (ax, ay) =
    abs(ax - mx) + abs(ay - my)

let rec proper_dir (x:int) (y:int) =
    if x = 0 then
        (0, sign y)
    elif y = 0 then
        (sign x, 0)
    elif x < 0 then
        let (x', y') = proper_dir (-x) y
        (-x', y')
    elif y < 0 then
        let (x', y') = proper_dir x (-y)
        (x', -y')
    else
        let d = gcd x y
        (x / d, y / d)

let dir (mx, my) (ax, ay): (int*int) =
    proper_dir (ax - mx) (ay - my)

let visibleFrom (monitor:int*int) (asteroids:List<int * int>) =
    let (mx, my) = monitor
    asteroids
    |> List.filter (fun asteroid -> let (ax, ay) = asteroid
                                    ax = mx && ay = my)

let emptyDirMap: Map<int*int, List<int*int>> =
    Map.empty

let addAsteroid (m: Map<int*int, List<int*int>>) (dir:int*int) (asteroid:int*int) =
    match m.TryFind dir with
    | Some asteroids -> m.Add (dir, asteroid::asteroids)
    | None -> m.Add (dir, asteroid::[])

let Answer =
    let asteroids = coOrds input
    let bestMonitor =
        asteroids
        |> List.map (fun monitor -> let visible =
                                        asteroids
                                        |> List.fold (fun m asteroid -> addAsteroid m (dir monitor asteroid) asteroid)
                                                     emptyDirMap
                                    (monitor, visible))
        |> List.maxBy (fun (monitor, visible) -> visible.Count)
    let (monitor, visible) = bestMonitor
    printfn "Best monitor: %A" monitor
    visible.Count - 1
