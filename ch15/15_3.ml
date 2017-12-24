(* 目的: リスト lst から n で割り切れる要素を除去する *)
(* remove_mod_0: int list -> int list *)
let remove_mod_0 lst n =
  List.filter (fun x -> x mod n <> 0) lst
;;

let test1 = remove_mod_0 [] 3 = [] ;;
let test2 = remove_mod_0 [2; 3; 4; 5] 2 = [3; 5] ;;


(* 目的: 2以上 n 以下の自然数のリストを受け取りそのリストに含まれる素数を返す *)
(* sieve: int list -> int list *)
let rec sieve lst = match lst with
    [] -> []
  | first :: rest ->
     let result = remove_mod_0 rest first in
     first :: (sieve result)
;;

(* テスト *)
let test3 = sieve [2] = [2] ;;
let test4 = sieve [2; 3; 4; 5; 6; 7; 8; 9; 10] = [2; 3; 5; 7] ;;


(* 目的: 受け取った自然数 n から 2以上 n 以下の自然数のリストを返す *)
(* two_to_n: int -> int list *)
let rec two_to_n n =
  if n = 2
  then [2]
  else List.append (two_to_n (n - 1)) [n]
;;

(*テスト *)
let test6 = two_to_n 2 = [2] ;;
let test7 = two_to_n 10 = [2; 3; 4; 5; 6; 7; 8; 9; 10] ;;


(* 目的: 受け取った自然数 n 以下の素数のリストを返す *)
(* prime: int -> int list *)
let prime n = 
  sieve (two_to_n n)
;;

(* テスト *)
let test8 = prime 2 = [2] ;;
let test9 = prime 10 = [2; 3; 5; 7] ;;
