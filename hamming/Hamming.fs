module Hamming

let hamming s1 s2 =
    if s1 <> s2 then 1 else 0

let matchStrands strandList1 strandList2 =
    match strandList1, strandList2 with
    | [], [] -> Some 0
    | s1, s2 when (Seq.length s1) <> (Seq.length s2) -> None
    | s1, s2 ->
        Seq.map2 hamming s1 s2
        |> Seq.sum
        |> Some

let toCharacterList strand = Seq.toList strand

let distance (strand1: string) (strand2: string) : int option =
    let strand1List = toCharacterList strand1
    let strand2List = toCharacterList strand2

    matchStrands strand1List strand2List
