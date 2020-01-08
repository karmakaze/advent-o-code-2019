module Day14

open System
open System.IO
open System.Text.RegularExpressions

let filename = "day14-input.txt"

let input =
    File.ReadLines(filename)

//let input = seq {
                //yield "10 ORE => 10 A"
                //yield "1 ORE => 1 B"
                //yield "7 A, 1 B => 1 C"
                //yield "7 A, 1 C => 1 D"
                //yield "7 A, 1 D => 1 E"
                //yield "7 A, 1 E => 1 FUEL"
                //}
//let input = seq {
                //yield "9 ORE => 2 A"
                //yield "8 ORE => 3 B"
                //yield "7 ORE => 5 C"
                //yield "3 A, 4 B => 1 AB"
                //yield "5 B, 7 C => 1 BC"
                //yield "4 C, 1 A => 1 CA"
                //yield "2 AB, 3 BC, 4 CA => 1 FUEL"
                //}

//let input = seq {
                //yield "157 ORE => 5 NZVS"
                //yield "165 ORE => 6 DCFZ"
                //yield "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
                //yield "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
                //yield "179 ORE => 7 PSHF"
                //yield "177 ORE => 5 HKGWZ"
                //yield "7 DCFZ, 7 PSHF => 2 XJWVT"
                //yield "165 ORE => 2 GPVTF"
                //yield "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
                //}

//let input = seq {
                //yield "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
                //yield "17 NVRVD, 3 JNWZP => 8 VPVL"
                //yield "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
                //yield "22 VJHF, 37 MNCFX => 5 FWMGM"
                //yield "139 ORE => 4 NVRVD"
                //yield "144 ORE => 7 JNWZP"
                //yield "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
                //yield "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
                //yield "145 ORE => 6 MNCFX"
                //yield "1 NVRVD => 8 CXFTF"
                //yield "1 VJHF, 6 MNCFX => 4 RFSQX"
                //yield "176 ORE => 6 VJHF"
                //}

//let input = seq {
                //yield "171 ORE => 8 CNZTR"
                //yield "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
                //yield "114 ORE => 4 BHXH"
                //yield "14 VRPVC => 6 BMBT"
                //yield "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
                //yield "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
                //yield "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
                //yield "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
                //yield "5 BMBT => 4 WPTQ"
                //yield "189 ORE => 9 KTJDG"
                //yield "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
                //yield "12 VRPVC, 27 CNZTR => 2 XDBXC"
                //yield "15 KTJDG, 12 BHXH => 5 XCVML"
                //yield "3 BHXH, 2 VRPVC => 7 MZWV"
                //yield "121 ORE => 7 VRPVC"
                //yield "7 XCVML => 6 RJRHP"
                //yield "5 BHXH, 4 VRPVC => 5 LTCX"
                //}

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    match m.Success with
    | true -> Some(List.tail [for g in m.Groups -> g.Value])
    | _    -> None

let split (l:List<_>) =
    let (sym:List<string>, num:List<int>) =  l |> List.fold (fun (l1:List<_>, l2:List<_>) x ->
                                                      if l1.Length < l2.Length then
                                                          (x::l1, l2)
                                                      else
                                                          (l1, (int x)::l2)
                                                  ) ([], [])
    match (sym, num) with
    | (makes::needs, count::units) ->
          let uses = List.zip needs units
                     |> List.fold (fun (m:Map<string, int>) (k, v) -> m.Add (k, v)) Map.empty
          (makes, (count, uses))
    | _ -> ("_", (0, Map.empty))

let itemCount (s:string) =
    match s with
    | Regex @"^([0-9]+) ([A-Z]+)$" [need; consume] ->
        (consume, int need)
    | _ -> invalidArg "count item" "failed to parse form"

type Store() =
    let mutable items: Map<string, int> = Map.empty

    member s.add (item, count) =
        if count < 0 then
            invalidArg "count" "should never be negative"
        elif count = 0 then s
        else
            match items.TryFind item with
            | Some total ->
                items <- items.Add(item, total + count)
            | None ->
                items <- items.Add(item, count)
            s

    member __.sub (item, count) =
        if count < 0 then
            invalidArg "count" "should never be negative"
        elif count = 0 then 0
        else
            match items.TryFind item with
            | Some total ->
                if total > count then
                    items <- items.Add(item, total - count)
                    0
                else
                    items <- items.Remove item
                    count - total
            | None -> count

    member s.getItems =
        items

let productionRules =
    input
    |> Seq.map (fun line ->
                      printfn "%A" line
                      match line with
                      | Regex @"^([^=]+) => ([0-9]+) ([A-Z]+)$" [consumes; count; product] ->
                          let store = Store()
                          consumes.Split ", " |> Array.toList
                                              |> List.map itemCount
                                              |> List.iter (fun (k, v) -> store.add(k, v) |> ignore)
                          (product, (int count, store.getItems))
                      | _ -> invalidArg "consumes => product" "failed to parse form")
    |> Seq.fold (fun (m:Map<string, int * Map<string, int>>) (k, v) -> m.Add(k, v)) Map.empty

let oreUnitsFor product count =
    let rec loop (ores:Store) (inventory:Store) (targets:Store) =
        if targets.getItems.IsEmpty then
            ores.getItems
        else
            let (target, targetCount) = targets.getItems |> Map.toSeq |> Seq.head
            targets.sub(target, targetCount) |> ignore
            printfn "making %A %A:" targetCount target

            if target = "ORE" then
                ores.add(target, targetCount) |> ignore
                loop ores inventory targets
            else
                let need = inventory.sub (target, targetCount)
                if need > 0 then
                    match productionRules.TryFind target with
                    | Some (productCount, uses) ->
                        // factor = need / productCount with upward rounding
                        let factor = (need + productCount - 1) / productCount
                        printfn "producing %A %A x%A" productCount target factor

                        inventory.add(target, productCount * factor - need) |> ignore
                        printfn "add %A %A to inventory" (productCount * factor - need) target

                        uses |> Map.toSeq |> Seq.iter (fun (item, itemCount) ->
                                                           printfn "add %A %A x%A to products" itemCount item factor
                                                           targets.add(item, itemCount * factor) |> ignore)
                        loop ores inventory targets
                    | _ ->
                        invalidArg "target" "is missing a production rule"
                else
                    printfn "consumed %A %A from inventory" targetCount target
                    loop ores inventory targets
    loop (Store()) (Store()) (Store().add(product, count))

let Answer =
    oreUnitsFor "FUEL" 1
