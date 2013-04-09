 (******************************
           Лозов Пётр
           Группа 171
            09.04.13
           Problem 72 
      Counting fractions №2
 Время выполнения - 36.28 секунд
 *******************************)

let d = 1000000

(* Возвращает массив простых чисел <= number *)
let create_array_prime number =
    let array_prime : int array = Array.zeroCreate (number / 2 + 2)
    let array_bool = Array.create number true
    let mutable point = 0
    for i = 1 to number - 1 do
        if array_bool.[i] = true then
            array_prime.[point] <- i + 1
            point <- point + 1
            for j in [i..(i + 1)..number - 1] do
                array_bool.[j] <- false
    array_prime   
    
let array_prime = d |> float |> sqrt |> int |> create_array_prime
(* Возвращает количество правильных дробей при фиксированном знаменателе = denominator *)
let quantity_of_partial_fractions denominator =
   
    (* Возвращает массив, состоящий из простых множителей, числа number *) 
    let prime_factors number =
        let array_prime_factors : int array = Array.zeroCreate 10
        let mutable i = 0
        let mutable j = 0
        let mutable number' = number
        while array_prime.[i] <> 0 && number' <> 1 do
            if number' % array_prime.[i] = 0 then
                while number' % array_prime.[i] = 0 do
                    number' <- number' / array_prime.[i]
                array_prime_factors.[j] <- array_prime.[i]
                j <- j + 1
            i <- i + 1
        if number' <> 1 then array_prime_factors.[j] <- number'
        array_prime_factors
   
    (* Возвращает длину массива array', при условии, что конец массива - это первый нулевой элемент *)
    let size_array (array' : int array) =
        let mutable i = 1
        let length_array = Array.length array'
        while array'.[i] <> 0 && i <> length_array do
            i <- i + 1
        i

    (* Возвращает следующее сочитание послле combination, в случае, если combination - максимальное сочетание, возвращает [| 0 |] *)
    let rec next_combination (combination : int array) point number =
        let mutable combination' = combination
        if combination'.[point] < number then 
            combination'.[point] <- combination'.[point] + 1
        else if point <> 0 then 
            combination' <- next_combination combination' (point - 1) (number - 1)
            combination'.[point] <- combination'.[point - 1] + 1
        else
            combination'.[0] <- 0
        combination'

    let mutable sum = denominator
    let array_prime_factors = prime_factors denominator
    let size_array_prime_factors = size_array array_prime_factors
    for i = 1 to size_array_prime_factors do
        let sign = 1 - 2 * (i % 2)
        let mutable combination = [| 1 .. i |] 
        while combination <> [| 0 .. i - 1 |] do
            let combination' = combination
            let a = (Array.fold (fun factor1 factor2 -> factor1 * factor2) 1 [| for j in 0 .. i - 1 -> array_prime_factors.[combination'.[j] - 1] |])
            sum <- sum + sign * denominator / a
            combination <- next_combination combination (i - 1) size_array_prime_factors
    sum
              
           
let mutable sum = 0L
for i = 2 to d do
   sum <- sum + (i |> quantity_of_partial_fractions |> int64)
printfn "%A" sum
