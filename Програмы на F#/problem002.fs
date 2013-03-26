(* Even Fibonacci numbers *)

let d = 4000000

let mutable sum = 0
let mutable i1 = 0
let mutable i2 = 1
while (i2 <= d) do
    let i = i2
    i2 <- i1 + i2
    i1 <- i
    if i2 % 2 = 0 then sum <- sum + i2

printfn "%A" sum