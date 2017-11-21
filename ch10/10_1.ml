(* int list は
   - []             空リスト、あるいは
   - first :: rest  最初の要素が first で残りのリストが rest
                    (rest は自己参照のケース)
   という形 *)

(* 目的: 昇順に並んでいる整数のリスト lst と整数 n を受け取り
   昇順を維持したまま n を lst に追加したリストを返す *)
(* insert: int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> n :: []
  | first :: rest ->
    if n <= first then
      n :: (first :: rest)
    else
      first :: (insert rest n)
;;


(* テスト *)
let test1 = insert [] 1 = [1] ;;
let test2 = insert [3] 1 = [1; 3] ;;
let test3 = insert [3] 5 = [3; 5] ;;
let test4 = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8] ;;
