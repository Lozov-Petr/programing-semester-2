 (******************************
           Лозов Пётр
           Группа 171
            09.04.13
           Problem 72 
      Counting fractions №1
    Время выполнения ~ 18 часов
 *******************************)

let d = 1000000

let rec GCD x y =
    if y = 0 then x else GCD y (x % y)

let mutable sum = 0L

for i = 2 to d do
    for j = 1 to i - 1 do
        if i % 2 <> 0 || j % 2 <> 0 then
            if (GCD i j) = 1 then sum <- sum + 1L

printfn "%A" sum       
