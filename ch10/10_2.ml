#use "10_1.ml" ;;

(* int list は
   - []             空リスト、あるいは
   - first :: rest  最初の要素が first で残りのリストが rest
                    (rest は自己参照のケース)
   という形 *)


(* 目的: 受け取った整数のリスト lst を昇順に整列したリストを返す *)
(* ins_sort: int list -> int list *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (ins_sort rest) first
;;


(* テスト *)
let test1 = ins_sort [] = [] ;;
let test2 = ins_sort [1; 2; 3] = [1; 2; 3] ;;
let test3 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8] ;;
