 (******************************
           Лозов Пётр
           Группа 171
            09.04.13
           Problem 73 
  Counting fractions in a range  
 Время выполнения - 1.39 секунды
 *******************************)
let d = 12000

let rec GCD x y =
    if y = 0 then x else GCD y (x % y)

let mutable sum = 0

for i = 3 to d do
    for j = i / 3 + 1 to i / 2 do
        if i % 2 <> 0 || j % 2 <> 0 then
            if (GCD i j) = 1 then sum <- sum + 1

printfn "%A" sum   
