(* 10001st prime *)
let d = 10001

let arr = Array.zeroCreate d
arr.[0] <- 2
arr.[1] <- 3
let mutable size = 2
let next_prime x = 
    let mutable prime = x
    let mutable is_prime = false
    while is_prime = false do 
        is_prime <- true
        prime <- prime + 2
        let mutable i = 1
        while (i < size) && (is_prime = true) do
            if prime % arr.[i] = 0 then is_prime <- false
            i <- i + 1
    prime

while (size < d) do
    arr.[size] <- (next_prime arr.[size - 1])
    size <- size + 1

printfn "%A" arr.[d - 1]


                      