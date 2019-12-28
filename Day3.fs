module Day3

open System.IO

    type Point = int * int
    type Line = Point * Point
    type Segment = int * Line

    let lr_h_line ((x1, y), (x2, _)) =
        if x1 < x2 then ((x1, y), (x2, y))
        else ((x2, y), (x1, y))

    let bu_v_line ((x, y1), (_, y2)) =
        if y1 < y2 then ((x, y1), (x, y2))
        else ((x, y2), (x, y1))

    let distance ((ax:int, ay:int), (bx:int, by:int)) = abs(ax - bx) + abs(ay - by)

    let intersect_v_segments (h_steps, h_line) (v_segments:List<Segment>) =
        let (h_point, _) = h_line
        let ((hx1, hy), (hx2, _)) = lr_h_line h_line
        v_segments
        |> List.fold (fun intersects v_segment ->
                         let (v_steps, v_line) = v_segment
                         let ((vx, vy1), (_, vy2)) = bu_v_line v_line
                         if hx1 < vx && vx < hx2 &&
                            vy1 < hy && hy < vy2 then
                                let (v_point, _) = v_line
                                let intersect = (vx, hy)
                                let steps = v_steps + distance(v_point, intersect) +
                                            h_steps + distance(h_point, intersect)
                                (steps, intersect)::intersects
                         else intersects)
                     []

    let intersect_h_segments (v_steps, v_line) (h_segments:List<Segment>) =
        let (v_point, _) = v_line
        let ((vx, vy1), (_, vy2)) = bu_v_line v_line
        h_segments
        |> List.fold (fun intersects h_segment ->
                         let (h_steps, h_line) = h_segment
                         let ((hx1, hy), (hx2, _)) = lr_h_line h_line
                         if hx1 < vx && vx < hx2 &&
                            vy1 < hy && hy < vy2 then
                               let (h_point, _) = h_line
                               let intersect = (vx, hy)
                               let steps = v_steps + distance(v_point, intersect) +
                                           h_steps + distance(h_point, intersect)
                               (steps, intersect)::intersects
                         else intersects)
                     []

    type Grid() =
        [<DefaultValue>]
        val mutable h_segments : List<Segment>
        [<DefaultValue>]
        val mutable v_segments : List<Segment>

        member this.init() =
            this.h_segments <- []
            this.v_segments <- []

        member this.HLines = this.h_segments
        member this.PrintHLines = printfn "%A" this.h_segments

        member this.place (segment:Segment) =
            let (_, (a, b)) = segment
            let (ax, ay) = a
            let (bx, by) = b
            if ax = bx then
                this.v_segments <- segment::this.v_segments
            elif ay = by then
                this.h_segments <- segment::this.h_segments
            else invalidArg "segment" "Segments must be horizontal or vertical" 

        member this.intersect (segment:Segment) =
            let (_, (a, b)) = segment
            let (ax, ay) = a
            let (bx, by) = b
            if ax = bx then
                intersect_h_segments segment this.h_segments
            elif ay = by then
                intersect_v_segments segment this.v_segments
            else invalidArg "segment" "Segments must be horizontal or vertical" 

    let filename = "day3-input.txt"

    let input =
        File.ReadLines(filename)
        |> Seq.map (fun x -> x.Split ',')
        |> Seq.map Array.toList
        |> Seq.toList
    //let input = ["R75,D30,R83,U83,L12,D49,R71,U7,L72".Split( ',') |> Array.toList
    //             "U62,R66,U55,R34,D71,R55,D58,R83".Split( ',')  |> Array.toList]

    // part 1

    let dirMag (command:string) =
        let dir = command.[0]
        let mag = int command.[1..]
        (dir, mag)

    let nextPoint (last:Point) (dir, mag) =
        let (lx, ly) = last
        if dir = 'L' then (lx - mag, ly)
        elif dir = 'R' then (lx + mag, ly)
        elif dir = 'U' then (lx, ly + mag)
        elif dir = 'D' then (lx, ly - mag)
        else invalidArg (string dir) "dir must be L, R, U, or D"

    let placefun ((grid:Grid), (steps:int), (last:Point)) (cmd:string) =
        let (dir, mag) = dirMag cmd
        let next = nextPoint last (dir, mag)
        grid.place (steps, (last, next))
        (grid, steps + (distance (last, next)), next)

    let interfun ((intersects:List<int *Point>), (grid:Grid), (steps:int), (last:Point)) (cmd:string) =
        let (dir, mag) = dirMag cmd
        let next = nextPoint last (dir, mag)
        (intersects @ grid.intersect (steps, (last, next)), grid, steps + distance (last, next), next)

    let Answer1 =
        let grid = new Grid()
        grid.init()
        input.[0] |> List.fold placefun (grid, 0, (0, 0)) |> ignore // mutates grid
        input.[1]
        |> List.fold interfun ([], grid, 0, (0, 0))
        |> fun (intersects, _, _, _) -> intersects
        |> List.fold (fun (d, sp) (s, (x, y)) ->
                         if d = -1 || s < d then (s, (x, y))
                         else (d, sp))
                     (-1, (0, 0))
