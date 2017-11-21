(* int list は
   - []             空リスト、あるいは
   - first :: rest  最初の要素が first で残りのリストが rest
                    (rest は自己参照のケース)
   という形 *)

(* 目的: 受け取った整数のリスト lst に含まれる偶数の要素をリストとして返す *)
(* even: int list -> int list *)
let rec even lst = match lst with
    [] -> []
  | first :: rest ->
    if first mod 2 = 0 then
      first :: even rest
    else
      even rest
;;


(* テスト *)
let test1 = even [] = [] ;;
let test2 = even [1] = [] ;;
let test3 = even [2] = [2] ;;
let test4 = even [1; 2; 3; 4; 5] = [2; 4] ;;
let test5 = even [2; 1; 6; 4; 7] = [2; 6; 4] ;;
