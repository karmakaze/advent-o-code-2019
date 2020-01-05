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

let quadrant (x:int, y:int) =
    if x >= 0 && y < 0 then 0
    elif x > 0 && y >= 0 then 1
    elif x <= 0 && y > 0 then 2
    elif x < 0 && y <= 0 then 3
    else invalidArg "(x, y)" "may not be (0, 0)"

let compareByDir ((x1:int, y1:int), vs1:List<int*int>) ((x2:int, y2:int), vs2:List<int*int>) =
    let q1 = quadrant (x1, y1)
    let q2 = quadrant (x2, y2)
    match compare q1 q2 with
    | 0 ->
        if q1 = 0 || q1 = 2 then
            let slope1 = double x1 / double y1
            let slope2 = double x2 / double y2
            compare slope2 slope1
        else
            let slope1 = (- double y1) / double x1
            let slope2 = (- double y2) / double x2
            compare slope2 slope1
    | c -> c

let quadrantSlope (dx:int, dy:int) =
    let q = quadrant (dx, dy)
    let s = match q with
                | 0 -> double dx / double dy
                | 1 -> - double dy / double dx
                | 2 -> double dx / double dy
                | 3 -> - double dy / double dx
    (q, s)

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
    let otherVisible = visible.Remove (0, 0)
    printfn "Best monitor: %A" monitor
    otherVisible |> Map.toList |> List.sortWith compareByDir
//                 |> List.map (fun ((dx, dy) as dir, asteroids) -> (quadrantSlope (dx, dy), dir, asteroids))
                 |> List.toSeq |> Seq.skip 199 |> Seq.take 1
