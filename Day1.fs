module Day1

open System.IO

let filename = "day1-input.txt"

let input =
    File.ReadLines(filename)
    |> Seq.map (fun x -> int x)

// part 1

let div3 x = x / 3
let sub2 x = x - 2

let fuelForModuleMass x = (div3 >> sub2) x

let Answer1 =
    input
    |> Seq.map fuelForModuleMass
    |> Seq.sum

// part 2

let fuelForFuelMass1 x =
    let fuel = (div3 >> sub2) x
    if fuel >= 0 then fuel
    else 0

let rec fuelForFuelMass acc fuel =
    if fuel = 0 then acc
    else fuelForFuelMass (acc + fuel) (fuelForFuelMass1 fuel)

let fuelForModuleMassWithFuel x = fuelForFuelMass 0 (fuelForModuleMass x)

let Answer2 =
    input
    |> Seq.map fuelForModuleMassWithFuel
    |> Seq.sum
