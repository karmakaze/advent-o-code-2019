module Day3

open System.IO

    type Point = int * int
    type Line = Point * Point

    let intersect_v_lines h_line v_lines =
        let ((hx1, hy), (hx2, _)) = h_line
        v_lines
        |> List.fold (fun intersects v_line ->
                         let ((vx, vy1), (_, vy2)) = v_line
                         if hx1 < vx && vx < hx2 &&
                            vy1 < hy && hy < vy2 then
                            (vx, hy)::intersects
                         else intersects)
                     []

    let intersect_h_lines v_line h_lines =
        let ((vx, vy1), (_, vy2)) = v_line
        h_lines
        |> List.fold (fun intersects h_line ->
                         let ((hx1, hy), (hx2, _)) = h_line
                         if hx1 < vx && vx < hx2 &&
                            vy1 < hy && hy < vy2 then
                            (vx, hy)::intersects
                         else intersects)
                     []

    let lr_h_line ((x1, y), (x2, _)) =
        if x1 < x2 then ((x1, y), (x2, y))
        else ((x2, y), (x1, y))

    let bu_v_line ((x, y1), (_, y2)) =
        if y1 < y2 then ((x, y1), (x, y2))
        else ((x, y2), (x, y1))

    type Grid() =
        [<DefaultValue>]
        val mutable h_lines : List<Line>
        [<DefaultValue>]
        val mutable v_lines : List<Line>

        member this.init() =
            this.h_lines <- []
            this.v_lines <- []

        member this.HLines = this.h_lines
        member this.PrintHLines = printfn "%A" this.h_lines

        member this.place (a:Point) (b:Point) =
            let (ax, ay) = a
            let (bx, by) = b
            if ax = bx then
                let line = bu_v_line (a, b)
                this.v_lines <- line::this.v_lines
            elif ay = by then
                let line = lr_h_line (a, b)
                this.h_lines <- line::this.h_lines
            else invalidArg "a b" "Lines must be horizontal or vertical" 

        member this.intersect (a:Point) (b:Point) =
            let (ax, ay) = a
            let (bx, by) = b
            if ax = bx then
                let line = bu_v_line (a, b)
                intersect_h_lines line this.h_lines
            elif ay = by then
                let line = lr_h_line (a, b)
                intersect_v_lines line this.v_lines
            else invalidArg "a b" "Lines must be horizontal or vertical" 

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

    let placefun ((grid:Grid), (last:Point)) (cmd:string) =
        let (dir, mag) = dirMag cmd
        let next = nextPoint last (dir, mag)
        grid.place last next
        (grid, next)

    let interfun ((intersects:List<Point>), (grid:Grid), (last:Point)) (cmd:string) =
        let (dir, mag) = dirMag cmd
        let next = nextPoint last (dir, mag)
        (intersects @ (grid.intersect last next), grid, next)

    let Answer1 =
        let grid = new Grid()
        grid.init()
        input.[0] |> List.fold placefun (grid, (0, 0)) |> ignore // mutates grid
        input.[1]
        |> List.fold interfun ([], grid, (0, 0))
        |> fun (intersects, _, _) -> intersects
        |> List.fold (fun (d, p) (x, y) ->
                         let dist = abs x + abs y
                         if d = -1 || dist < d then (dist, (x, y))
                         else (d, p))
                     (-1, (0, 0))
