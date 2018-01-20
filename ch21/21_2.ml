(* 目的: リスト lst から n で割り切れる要素を除去する *)
(* remove_mod_0: int list -> int list *)
let remove_mod_0 lst n =
  List.filter (fun x -> x mod n <> 0) lst
;;


(* 目的: 2以上 n 以下の自然数のリストを受け取りそのリストに含まれる素数を返す *)
(* sieve: int list -> int list *)
let rec sieve lst =
  let length = List.length lst in
  (print_string "length: ";
   print_int length;
   print_newline ();
   match lst with
     [] -> []
   | first :: rest ->
      let result = remove_mod_0 rest first in
      first :: (sieve result)   
  )
;;

(* テスト *)
let test1 = sieve [2] = [2] ;;
let test2 = sieve [2; 3; 4; 5; 6; 7; 8; 9; 10] = [2; 3; 5; 7] ;;
