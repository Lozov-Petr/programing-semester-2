(* Smallest multiple *)

let d = 20

let rec GCD x y = if x = 0 then y else (GCD (y % x) x)

let mutable mul = 1
for i = 2 to d do mul <- i / (GCD i mul) * mul

printfn "%A" mul 