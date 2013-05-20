 (******************************
           Лозов Пётр
           Группа 171
            10.05.13
         long arithmetic
 *******************************)

type long_number =

    val private sign : bool
    val private number : int list

    static member private max_digit = 1000000000
    
    static member private len_max_digit = 9

    new() = {sign = true; number =[0]}
    
    private new(sign' : bool, list : int list) = 
        let list = if list <> [] then list else [0]
        let sign' = if list <> [0] then sign' else true
        {sign = sign'; number = list}

    new(str : string) =
        let len_str = String.length str
        let rec string_to_list num_sym acc =
            match num_sym with
            | n when n >= len_str - 1 -> if acc <> [] then acc else [0]
            | _ ->  
                let next_num_sym = num_sym + long_number.len_max_digit
                let digit = int str.[num_sym .. next_num_sym - 1]
                string_to_list next_num_sym (if digit = 0 && acc = [] then [] else digit :: acc)
        let first_num_symb = if str.[0] = '-' then 1 else 0
        let second_num_symb = (len_str - first_num_symb) % long_number.len_max_digit + first_num_symb
        let first_digit = if first_num_symb = second_num_symb then 0 else int str.[first_num_symb .. second_num_symb - 1]
        {
            sign = (first_num_symb = 0)
            number = string_to_list second_num_symb (if first_digit = 0 then [] else [first_digit])
        }

    new(a : int) =
        let a' = a / long_number.max_digit
        {
            sign = a >= 0
            number = (a % long_number.max_digit) :: (if a' <> 0 then [a'] else [])
        }

    new(a : int64) =
        let rec create_list a acc =
            if a <> 0L then create_list (a / (int64 long_number.max_digit)) (int (a % (int64 long_number.max_digit)) :: acc)
            else List.rev acc
        {
            sign = a >= 0L
            number = create_list (abs a) []
        }

    member this.print =
        let rec print list is_first_print =
            match list with
            | [] -> ()
            | hd :: tl ->
                if is_first_print = false then printf "%s" (String.replicate (long_number.len_max_digit - (String.length (string hd))) "0")
                printf "%d " hd
                print tl false
        if this.sign = false then printf "-"
        print (List.rev this.number) true
        printfn ""

    static member (~-) (a : long_number) = new long_number(a.sign = false, a.number)

    member this.abs = new long_number(true, this.number)

    static member private add (a : long_number, b : int) =
        let rec add list carry acc =
            if carry <> 0 then
                match list with
                | [] -> List.rev (carry :: acc)
                | hd :: tl ->
                    let new_hd = hd + carry
                    add tl (new_hd / long_number.max_digit) ((new_hd % long_number.max_digit) :: acc)
            else (List.rev acc) @ list
        new long_number(a.sign, add a.number (abs b) [])
    
    static member private sub(a : long_number, b : int) =
        let rec sub list carry acc =
            let rec plus_max_digit n i = if n >=0 then (n, i) else plus_max_digit (n + long_number.max_digit) (i + 1)
            if carry <> 0 then
                match list with
                | [] -> List.rev (long_number.del_zeros(acc))
                | hd :: tl ->
                    let (new_hd, i) = plus_max_digit (hd - carry) 0
                    sub tl i (new_hd :: acc)
            else (List.rev (long_number.del_zeros(acc))) @ list
        new long_number(a.sign, sub a.number (abs b) [])

    static member (+) (a : int, b : long_number) = b + a

    static member (-) (a : int, b : long_number) = -(b - a)
        
    static member (+) (a : long_number, b : int) =
        if a.sign = (b >= 0) then long_number.add(a, b) else long_number.sub(a, -b)

    static member (-) (a : long_number, b : int) =
        if a.sign = (b >= 0) then 
            if a >= new long_number(b) then long_number.sub(a, b) else -long_number.sub(new long_number(b), a)
        else long_number.add(a, -b)
   
    static member private add (a : long_number, b : long_number) =
        let rec add list1 list2 carry acc =
            match list1, list2 with
            | [], [] -> List.rev (if carry = 0 then acc else carry :: acc)
            | [], list -> (List.rev acc) @ ((new long_number(true, list)) + carry).number
            | list, [] -> (List.rev acc) @ ((new long_number(true, list)) + carry).number
            | hd1 :: tl1, hd2 :: tl2 ->
                 let new_hd = hd1 + hd2 + carry
                 add tl1 tl2 (new_hd / long_number.max_digit) ((new_hd % long_number.max_digit) :: acc)
        new long_number(a.sign, add a.number b.number 0 [])

    static member private sub (a : long_number, b : long_number) =
        let rec plus_max_digit n i = if n >= 0 then (n, i) else plus_max_digit (n + long_number.max_digit) (i + 1)
        let rec sub list1 list2 (carry : int) acc =
            match list1, list2 with
            | [], [] -> List.rev (long_number.del_zeros(acc))
            | list, [] -> 
                let tl = (new long_number(true, list) - carry).number
                let tl = if tl = [0] then [] else tl
                (List.rev acc) @ tl
            | hd1 :: tl1, hd2 :: tl2 -> 
                let (new_hd, i) = plus_max_digit (hd1 - hd2 - carry) 0
                sub tl1 tl2 i (new_hd :: acc)
            | _ -> []
        new long_number(a.sign, sub a.number b.number 0 [])

    static member (+) (a : long_number, b : long_number) =
        if a.sign = b.sign then long_number.add(a, b) 
        else
            if a.abs > b.abs then
                long_number.sub(a, -b)
            else
                -long_number.sub(-b, a)

    static member (-) (a : long_number, b : long_number) =
        if a.sign = b.sign then
            if a.abs > b.abs then
                long_number.sub(a, b)
            else
                -long_number.sub(b, a)
        else long_number.add(a, -b)

    static member private del_zeros(a : int list) = 
        let rec del_zeros list =
            match list with
            | [] -> []
            | hd :: tl -> if hd = 0 then del_zeros tl else list
        del_zeros a

    static member compare (a : long_number, b : long_number) =
        let rec compare list1 list2 =
            match list1, list2 with
            | [], [] -> 0
            | list, [] -> 1
            | [], list -> -1
            | hd1 :: tl1, hd2 :: tl2 ->
                if hd1 > hd2 then 1
                else if hd1 < hd2 then -1
                else compare tl1 tl2 
        let len_a = List.length a.number
        let len_b = List.length b.number
        if a.sign <> b.sign then (if a.sign then 1 else -1)
        else if len_a <> len_b then (if (len_a > len_b) = a.sign then 1 else -1)
        else compare (List.rev a.number) (List.rev b.number) 

    static member private compare_to_obj(a : long_number, b : obj) = 
        match b with
        | :? long_number as b' -> long_number.compare(a, b')
        | _ -> long_number.compare_to_obj(a, new long_number(string b))

    interface System.IComparable with
        member this.CompareTo(num) =
            match num with
            | :? long_number as num -> long_number.compare(this, num)
            | _ -> long_number.compare(this, new long_number(string num))

    override this.GetHashCode() = this.ToString().GetHashCode()
    override this.Equals(num) = long_number.compare_to_obj(this, num) = 0

    static member (*) (a : long_number, b : long_number) =
        let rec mult list1 list2 n1 n2 acc =
            match list1, list2 with
            | [], _ -> acc
            | _ :: tl, [] -> mult tl b.number (n1 + 1) 0 acc
            | hd1 :: tl1, hd2 :: tl2 -> 
                let slog = new long_number(true, [for i in [1 .. n1 + n2] -> 0] @ (new long_number((int64 hd1) * (int64 hd2))).number)
                mult list1 tl2 n1 (n2 + 1) (acc + slog)
        new long_number(a.sign = b.sign, (mult a.number b.number 0 0 (new long_number())).number) 

    static member (*) (a : long_number, b) = a * (new long_number(string b))

    static member factorial(n : int) =
        let rec fact n acc =
            if n > 1 then fact (n - 1) (acc * (new long_number(n)))
            else acc
        fact n (new long_number(1))

let test1 = long_number.factorial(100) * long_number.factorial(100) (* Посчитано верно, проверено в Wolfram Alpha *)
test1.print
let test2 = new long_number("100000000000000000000000000000000000") - new long_number("12345689876532")
test2.print

let a = new long_number("2")
let b = new long_number("1")

(* Тест правельного выбора операции *)
(a + b).print
(a + -b).print
(-a + b).print
(-a + -b).print
(a - b).print
(a - -b).print
(-a - b).print
(-a - -b).print
(a + 1).print
(a + -1).print
(-a + 1).print
(-a + -1).print
(a - 1).print
(a - -1).print
(-a - 1).print
(-a - -1).print
