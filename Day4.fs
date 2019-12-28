module Day4

open System.IO

//open System.Collections.Generic

let range = (372304, 847060)
let check num =
    let mutable has_doubled = false
    let rec check_place num place =
        let tens = (num / (place * 10)) % 10
        let ones = (num / place) % 10
        if tens > ones then false
        else
            if not has_doubled && tens = ones then
                has_doubled <- true
                if place < 10000 && tens = (num / (place * 100)) % 10 then
                    has_doubled <- false
                if place > 1 && ones = (num / (place / 10)) % 10 then
                    has_doubled <- false
            if place = 1 then has_doubled
            else check_place num (place / 10)
    check_place num 10000

let potentials (start, limit) =
    seq {
        for num in start..limit do
            if check num then
                yield num
        }

let Answer =
    potentials range
    |> Seq.fold (fun (s:Set<int>) x -> s.Add(x)) Set.empty
    |> Set.count
