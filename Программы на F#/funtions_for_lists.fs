 (******************************
           Лозов Пётр
           Группа 171
            22.04.13
       functions for lists
 *******************************)

let rec addToEnd list elem =
    match list with
    | [] -> [elem]
    | hd :: tl -> hd :: (addToEnd tl elem) 
    
let rec append list1 list2 =
    match list1 with
    | [] -> list2
    | hd :: tl -> hd :: (append tl list2)

let reverse list =
    let rec reverse' list new_list =
        match list with
        | [] -> new_list
        | hd :: tl -> reverse' tl (hd :: new_list)
    reverse' list []
        
let rec find func list =
    match list with
    | [] -> None
    | hd :: tl -> 
        if func hd then Some hd 
        else find func tl
         
let map func list =
    List.foldBack (fun x acc -> (func x) :: acc) list []


let test = 
    addToEnd [1..10] 11 = [1..11]
    && append [1..5] [6..9] = [1..9]
    && reverse [1..9] = [9..-1..1]
    && find (fun x -> x > 8) [1..8] = None
    && find (fun x -> x > 8) [1..20] = Some 9
    && map (fun x -> x * x) [1..5] = [for i in [1..5] -> i * i] 
    
printfn "%A" test

