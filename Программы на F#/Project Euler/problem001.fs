(* Multiples of 3 and 5 *)

let d = 1000

let mutable sum = 0
for i = 3 to d - 1 do
    if (i % 3 = 0) || (i % 5 = 0) then sum <- sum + i
printfn "%A" sum 