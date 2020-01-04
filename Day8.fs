module Day8

open System.IO

let filename = "day8-input.txt"

let width = 25
let height = 6
let input = File.ReadLines(filename) |> Seq.head
//let width = 3
//let height = 2
//let input = "123456789012"

type Layer(width:int, height:int, data:string) =
    member layer.count ch =
        data |> Seq.filter ((=) ch) |> Seq.length

    member layer.print =
        for row in 0 .. (height-1) do
            let offset = row * width
            printfn "Layer row %A" data.[offset .. (offset + width - 1)]

type Image private () =
    [<DefaultValue>]
    val mutable layers: List<Layer>

    new(width:int, height:int, data:string) as im =
        Image()
        then
            im.layers <- List.empty
            for i in 1 .. (data.Length / (width * height)) do
                let offset = (i - 1) * width * height
//              printfn "Image layer %A" data.[offset .. (offset + width * height - 1)]
                im.layers <- im.layers @ [Layer(width, height, data.[offset .. (offset + width * height - 1)])]

    member im.printLayers =
        for layer in im.layers do
            layer.print
        ()

let Answer =
    let image = Image(width, height, input)
    image.layers
    |> List.minBy (fun layer -> layer.count '0')
    |> (fun layer -> (layer.count '1') * (layer.count '2'))
