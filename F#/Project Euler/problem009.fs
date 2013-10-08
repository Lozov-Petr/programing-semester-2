(* Special Pythagorean triplet *)

let mutable a = 0
let mutable b = 0
let mutable is_great = false
while (a < 500) && (is_great = false) do
    a <- a + 1
    b <- 0
    while (b < 500) && (is_great = false) do
        b <- b + 1
        is_great <- (1000 * (a + b - 500) = a * b)

printfn "%A" (a * b * (1000 - a - b))