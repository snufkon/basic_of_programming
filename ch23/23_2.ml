#use "23_1.ml";;

(* 目的: ヒープを先頭から順番に取り出しリストにして返す *)
(* make_list (int, unit) Heap.t -> int list -> int list *)
let rec make_list heap lst =
  try
    let ((key, _), rest_heap) = Heap.split_top heap in
    make_list rest_heap (key :: lst)
  with Heap.Empty ->
    lst
;;
 

(* 目的: ヒープソートを行う *)
(* heap_sort: 'a list -> 'a list *)
let heap_sort lst = match lst with 
    [] -> []
  | first :: rest -> 
     let size = List.length lst in
     let heap = (Heap.create size first ()) in
     List.map
       (fun x ->
         Heap.insert heap x ()
       )
       lst;
     make_list heap []
;;

 
(* テスト *) 
let test1 = heap_sort [] = [] 
let test2 = heap_sort [1] = [1] 
let test3 = heap_sort [1; 2] = [2; 1] 
let test4 = heap_sort [2; 1] = [2; 1] 
let test5 = heap_sort [5; 3; 8; 1; 7; 4] = [8; 7; 5; 4; 3; 1]
