(* 目的: 関数 f, 初期値 init と リスト lst を受け取り、
   init からはじめてリストの要素を「左から」順に f を施し込む *)
(* fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f init lst = match lst with
    [] -> init
  | first :: rest -> fold_left f (f init first) rest
;;

(* テスト *)
let test1 = fold_left (+) 0 [1; 2; 3] = 6 ;;
