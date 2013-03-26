(* Summation of primes *)

let d = 2000000

let arr_bool = Array.create d true
let mutable point_bool = 1
let mutable sum = 0L
while point_bool <> d do
    if arr_bool.[point_bool] = true then
        sum <- sum + (int64 point_bool) + 1L
        for i in point_bool..(point_bool + 1)..(d - 1) do
            arr_bool.[i] <- false
    point_bool <- point_bool + 1

printfn "%A" sum

