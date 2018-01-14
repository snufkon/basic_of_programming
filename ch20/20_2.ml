#use "20_1.ml" ;;

(* 目的: rb_tree_t 型の赤黒木を受け取り、以下を行う関数
  子と孫の頂点が両方とも赤になっているかを調べる

  両方とも赤の場合は、木の再構成を行い赤の連続を解消した木を返す
  それ以外は、受け取った木をそのまま返す
*)
(* balance: rb_tree_t -> rb_tree_t *)
let balance rb_tree = match rb_tree with
    Node (Node (Node (a, xa, xb, Red, b),
                ya, yb, Red,
                c),
          za, zb, Black,
          d)
  | Node (Node (a,
                xa, xb, Red,
                Node (b, ya, yb, Red, c)),
          za, zb, Black,
          d)
  | Node (a,
          xa, xb, Black,
          Node (Node (b, ya, yb, Red, c),
                za, zb, Red,
                d))
  | Node (a,
          xa, xb, Black,
          Node (b,
                ya, yb, Red,
                Node (c, za, zb, Red, d)))
  -> Node (Node (a, xa, xb, Black, b),
           ya, yb, Red,
           Node (c, za, zb, Black, d))
  | _ -> rb_tree
;;


(* テストデータ *)
let rb_tree1 = Node (Node (Node (Empty, 10, "x", Red, Empty),
                           13, "y", Red,
                           Empty),
                     15, "z", Black,
                     Empty) ;;

let rb_tree2 = Node (Node (Empty,
                           10, "x", Red,
                           Node (Empty, 13, "y", Red, Empty)),
                     15, "z", Black,
                     Empty) ;;

let rb_tree3 = Node (Empty,
                     10, "x", Black,
                     Node (Node (Empty, 13, "y", Red, Empty),
                           15, "z", Red,
                           Empty)) ;;

let rb_tree4 = Node (Empty,
                     10, "x", Black,
                     Node (Empty,
                           13, "y", Red,
                           Node (Empty, 15, "z", Red, Empty)))
;;             


let result_rb_tree = Node (Node (Empty, 10, "x", Black, Empty),
                           13, "y", Red,
                           Node (Empty, 15, "z", Black, Empty))
;;


(* テスト *)
let test1 = balance rb_tree1 = result_rb_tree ;;
let test2 = balance rb_tree2 = result_rb_tree ;;
let test3 = balance rb_tree3 = result_rb_tree ;;
let test4 = balance rb_tree4 = result_rb_tree ;;
let test5 = balance Empty = Empty ;;
