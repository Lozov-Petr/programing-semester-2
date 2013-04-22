 (******************************
           Лозов Пётр
           Группа 171
            22.04.13
         Двоичное дерево
 *******************************)

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
    | Node(num, left, right) when num = elem -> true
    | Node(num, left, right) when num > elem -> exist left elem
    | Node(num, left, right) -> exist right elem

let rec delete tree elem =

    (* Если tree = Node(number, Empty, Empty), то заменяме на Leaf number*)
    let node_empty_empty tree =
        match tree with
        | Node(num, left, right) when left = Empty && right = Empty -> Leaf num
        | _ -> tree

    match tree with
    | Empty -> Empty
    | Leaf num when num = elem -> Empty
    | Leaf num -> Leaf num
    | Node(num, left, right) when num > elem -> node_empty_empty (Node(num, delete left elem, right))
    | Node(num, left, right) when num < elem -> node_empty_empty (Node(num, left, delete right elem))
    | Node(num, left, right) when left = Empty -> delete right elem
    | Node(num, left, right) when right = Empty -> delete left elem
    | Node(num, left, right) ->
        
        let rec min_elem_int_tree tree elem =
                match tree with
                | Node(num, left, right) -> min_elem_int_tree left num
                | Leaf num -> num
                | Empty -> elem 
        
        let min = min_elem_int_tree right num (* Самый левый элемент в правом поддереве *)
        delete (Node(min, left, delete right min)) elem 


let rec delete_list tree list =
    match list with
    | [] -> tree
    | hd :: tl -> delete_list (delete tree hd) tl

let print_tree tree =
    let rec print_tree' tree =
        match tree with
        | Node(num, left, right) ->
            printf "%A (" num
            print_tree' left
            printf ", " 
            print_tree' right
            printf ")" 
        | Leaf num -> printf "%A" num
        | Empty -> printf "E"
    print_tree' tree
    printfn ""
        
   
let tree = delete_list (insert_list Empty [2; 0; 4; -1; 1; 3; 5]) [-1; 1; 3; 5]

print_tree tree