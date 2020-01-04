module Day8

open System.IO
open System

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

    member layer.string = data

    member layer.print =
        for row in 0 .. (height-1) do
            let offset = row * width
            printfn "Layer row %A" data.[offset .. (offset + width - 1)]

    member layer.printBlack =
        for row in 0 .. (height-1) do
            let offset = row * width
            let black = data.[offset .. (offset + width - 1)].Replace("0", " ")
            printfn "Layer row %A" black

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

    member im.render =
        let layerStrings:List<List<char>> = im.layers |> List.map (fun layer -> layer.string |> Seq.toList)
        layerStrings
        |> List.transpose
        |> List.map (fun pixels -> let visible = pixels |> List.filter (fun pixel -> pixel <> '2')
                                   let vis_or_trans = visible @ ['2']
                                   vis_or_trans |> List.head)
        |> List.toSeq |> String.Concat
        |> (fun data -> Image(width, height, data))

    member im.printLayers =
        for layer in im.layers do
            layer.print
        ()

    member im.printBlack =
        for layer in im.layers do
            layer.printBlack
        ()

let Answer =
    let image = Image(width, height, input)
    let rendered = image.render
    rendered.printBlack
    rendered
