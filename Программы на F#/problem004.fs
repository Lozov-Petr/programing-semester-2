(* Largest palindrome product *)

let mutable max_pol = 0
for i = 999 downto 900 do
    for j = 999 downto 900 do
        let x = i * j
        if x / 100000 = x % 10 then
            let x1 = x / 10 - (x / 100000) * 10000
            if x1 / 1000 = x1 % 10 then
                let x2 = x1 / 10 - (x1 / 1000) * 100
                if (x2 / 10 = x2 % 10) && (max_pol < x) then max_pol <- x
                
printfn "%A" max_pol      