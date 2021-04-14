module Hamming

let compare (a: char) (b: char) : int = 1

let charList (str: string) = Seq.toList str

let list_tail (l : List<char>): List<char> =
    match l with
    | [] -> l
    | head :: tail -> tail
    

let compare_head (element : char) (list : List<char>) : int =
    match list with
    | [] -> 0
    | head :: tail -> compare element head
    

let sum (a: int) (b: int) : int = a + b

let rec head_tail (list1: List<char>) (list2: List<char>) : int =
    let tail_2 = list_tail list2
    match list1 with
    | [] -> 0
    | head :: tail -> (compare_head head list2) + head_tail(tail, tail_2)
    


let distance (strand1: string) (strand2: string) : int option = head_tail strand1 strand2
