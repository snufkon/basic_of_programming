#use "20_3.ml" ;;

(* 目的: rb_tree_t 型の赤黒木とキーを受け取り、キーに対応する値を返す *)
(* みつからなければ例外 Not_found を起こす *)
(* search: ('a, 'b) rb_tree_t -> 'a -> 'b *)
let rec search rb_tree k = match rb_tree with
    Empty -> raise Not_found
  | Node (left, key, value, color, right) ->
     if k = key then
       value
     else if k < key then
       search left k
     else search right k
;;


(* テスト *)
let rb_tree0 = Empty ;;
let rb_tree1 = insert Empty 10 "x" ;;
let rb_tree2 = insert rb_tree1 13 "y" ;;
let rb_tree3 = insert rb_tree2 15 "z" ;;

let test1 = search rb_tree3 10 = "x" ;;
let test2 = search rb_tree3 13 = "y" ;;
let test3 = search rb_tree3 15 = "z" ;;
let test4 = (try
               search rb_tree3 18
             with Not_found -> "") = "" ;;
