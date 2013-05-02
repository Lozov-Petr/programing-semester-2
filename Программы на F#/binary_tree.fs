 (******************************
           Лозов Пётр
           Группа 171
            02.05.13
         Двоичное дерево
 *******************************)

open System

type BinaryTree<'a> =
    | Empty
    | Leaf of 'a
    | Node of ('a * BinaryTree<'a> * BinaryTree<'a>)

let rec insert tree elem =
    match tree with
    | Empty -> Leaf elem
    | Leaf num when elem < num -> Node(num, Leaf elem, Empty)
    | Leaf num -> Node(num, Empty, Leaf elem)
    | Node(num, left, right) when elem < num -> Node(num, insert left elem, right)
    | Node(num, left, right) -> Node(num, left, insert right elem)

let rec insert_list tree list =
    match list with
    | [] -> tree
    | hd :: tl -> insert_list (insert tree hd) tl

let rec exist tree elem =
    match tree with
    | Empty -> false
    | Leaf num -> num = elem
    | Node(num, left, right) when elem = num -> true
    | Node(num, left, right) when elem < num -> exist left elem
    | Node(num, left, right) -> exist right elem

let rec delete tree elem =

    (* Если tree = Node(number, Empty, Empty), то заменяем на Leaf number *)
    let node_empty_empty tree =
        match tree with
        | Node(num, Empty, Empty) -> Leaf num
        | _ -> tree

    match tree with
    | Leaf num when num = elem -> Empty
    | Node(num, left, right) when elem < num -> node_empty_empty (Node(num, delete left elem, right))
    | Node(num, left, right) when elem > num -> node_empty_empty (Node(num, left, delete right elem))
    | Node(num, Empty, right) -> delete right elem
    | Node(num, left, Empty) -> left
    | Node(num, left, right) ->
        
        let rec max_elem_in_tree tree elem =
                match tree with
                | Node(num, left, right) -> max_elem_in_tree right num
                | Leaf num -> num
                | Empty -> elem 
        
        let max = max_elem_in_tree left num (* Самый правый элемент в левом поддереве *)
        delete (Node(max, delete left max, right)) elem 
    | _ -> tree

let rec delete_list tree list =
    match list with
    | [] -> tree
    | hd :: tl -> delete_list (delete tree hd) tl

let print_tree tree =

    let rec print_tree' tree list acc which_tree = 
        (* which_tree = 0  <=>  текущее дерево - начальное *)
        (* which_tree = -1 <=>  текущее дерево - левое *)
        (* which_tree = 1  <=>  текущее дерево - правое *)
        let rec print_spaces num =
            match num with
            | 1 -> printf " "
            | _ ->
                printf " "
                print_spaces (num - 1)

        let rec print_sticks list =
            match list with
            | hd :: [] -> print_spaces hd
            | hd :: tl ->
                print_spaces hd
                printf "|"
                print_sticks tl
            | [] -> ()

        match tree with
        | Empty -> 
            match which_tree with
            | 0 -> printfn "Empty"
            | _ -> printfn ""
        | Leaf num -> 
            match which_tree with
            | 0 -> printfn "(%A)" num
            | -1 -> printfn "\-(%A)" num 
            | 1 -> printfn "-(%A)" num
            | _ -> ()
        | Node (num, left, right) ->
            let acc' = 
                match which_tree with
                | 0 -> 
                    printf " (%A)*" num
                    3 + String.length (string num)
                | -1 ->
                    printf "\-(%A)*" num
                    acc + 4 + String.length (string num)
                | 1 ->
                    printf "-(%A)*" num
                    if acc = 0 then 3 + String.length (string num)
                    else 4 + acc + String.length (string num)
                | _ -> 0
            if left <> Empty then 
                let list' = list @ [acc']
                print_tree' right list' 0 1
                print_sticks list'
                printfn "|"
                print_sticks list'
                print_tree' left list acc' -1
            else
                 print_tree' right list acc' 1

    print_tree' tree [] 0 0
    printfn ""
                                                
let tree = insert_list (Leaf 0) [-4; 4; -6; -2; 2; 6] 
  
let test = 
    (insert (Node(5, Leaf 2, Empty)) 8) = Node(5, Leaf 2, Leaf 8) 
    && tree = Node(0, Node(-4, Leaf -6, Leaf -2), Node(4, Leaf 2, Leaf 6))
    && exist tree 2 = true
    && exist tree 5 = false
    && delete (insert_list Empty [1; 2; 1; 0; 2; 1]) 1 = Node(0, Empty, Node(2, Empty, Leaf 2))
    && delete_list tree [0; -4; 4] = Node(-2, Leaf -6, Node(2, Empty, Leaf 6))


printfn "test = %A\n" test

let beautiful_tree height = 

    let invol a b = int ((float a) ** (float b))

    let list1 = [for i in [(height - 1)..(-1)..0] -> invol 2 i]
    let list2 = List.map (fun x -> [x..(2 * x)..(invol 2 height)]) list1
    let list3 = List.foldBack (fun x acc -> x @ acc) list2 []
    insert_list Empty list3

let ugly_tree size = 
    let rand = Random()
    insert_list Empty [for i in [1..size] -> (rand.Next()) % size] 

let tree' = beautiful_tree 6
let tree'' = ugly_tree 63

print_tree tree'

print_tree tree''
