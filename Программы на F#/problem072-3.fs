 (******************************
           ����� ϸ��
           ������ 171
            09.04.13
           Problem 72 
      Counting fractions �3
 ����� ���������� - 0.34 �������
 *******************************)
let start = System.DateTime.Now
let d = 1000000
let mutable sum = 0L
let array_fi_number = Array.create (d + 1) 1L
(* fi - ������� ������ *)
for i in 2 .. d do
    if array_fi_number.[i] = 1L then
        for j in i .. i .. d do
            let mutable j' = int64 j
            while j' % ((int64 i) * (int64 i)) = 0L do
                array_fi_number.[j] <- array_fi_number.[j] * (int64 i)
                j' <- j' / (int64 i)
            array_fi_number.[j] <- array_fi_number.[j] * (int64 (i - 1))
    sum <- sum + array_fi_number.[i]
printfn "%A\n%A" sum (System.DateTime.Now - start)  