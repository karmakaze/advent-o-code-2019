module File1

type MyClass private() =
    [<DefaultValue>] val mutable v:int

    new(v:int) as this = MyClass() then this.v <- v

    member m.inc =
        m.v <- m.v + 1

    member m.printVars =
      printfn "v=%A" m.v

let Run =
    let o1 = new MyClass(1)
    o1.printVars
    o1.inc
    o1.printVars

    let o2 = new MyClass(2)
    o2.printVars
    o2.inc
    o2.printVars
