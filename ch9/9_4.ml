(* int list は
   - []             空リスト、あるいは
   - first :: rest  最初の要素が first で残りのリストが rest
                    (rest は自己参照のケース)
   という形 *)

(* 目的: 受け取った整数のリスト lst の長さを返す *)
(* length: int list -> int *)
let rec length lst = match lst with
    [] -> 0
  | first :: rest -> length rest + 1
;;


(* テスト *)
let test1 = length [] = 0 ;;
let test2 = length [3] = 1 ;;
let test3 = length [1; 5; 10] = 3 ;;
let test4 = length [2; 1; 6; 4; 7] = 5 ;;
