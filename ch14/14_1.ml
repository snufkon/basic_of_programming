(* 目的: リスト lst の中から条件 p を満たす要素のみを取り出す *)
(* filter ('a -> bool) -> 'a list -> 'a list *)
let rec filter p lst = match lst with
    [] -> [] 
  | first :: rest ->
     if p first then
       first :: filter p rest
     else
       filter p rest
;;


(* 目的: 整数 n が偶数かどうかを調べる *)
(* is_even: int -> bool *)
let is_even n =
  n mod 2 = 0
;;


(* 目的: 受け取った整数のリスト lst に含まれる偶数の要素をリストとして返す *)
(* even: int list -> int list *)
let even lst =
  filter is_even lst
;;


(* テスト *)
let test1 = even [] = [] ;;
let test2 = even [1] = [] ;;
let test3 = even [2] = [2] ;;
let test4 = even [1; 2; 3; 4; 5] = [2; 4] ;;
let test5 = even [2; 1; 6; 4; 7] = [2; 6; 4] ;;
