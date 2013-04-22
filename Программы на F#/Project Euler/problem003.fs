(* Largest prime factor *)

let mutable d1 = 600851
let mutable d2 = 475143

if ((d1 % 2) * 1000000 + d2) % 2 = 0 then
    d2 <- ((d1 % 2) * 1000000 + d2) / 2
    d1 <- d1 / 2
    
let mutable i = 1
while (d1 <> 0) || (d2 <> 1) do
    i <- i + 2
    if ((d1 % i) * 1000000 + d2) % i = 0 then
        d2 <- ((d1 % i) * 1000000 + d2) / i
        d1 <- d1 / i

printfn "%A" i



