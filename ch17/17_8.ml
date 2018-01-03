#use "tree.ml" ;;

(* 木の例 *)
let tree1 = Empty ;;
let tree2 = Leaf (3) ;;
let tree3 = Node (tree1, 4, tree2) ;;
let tree4 = Node (tree2, 5, tree3) ;;


(* 目的: tree_t 型 の木を受け取り、木の深さを返す *)
(* tree_depth: tree_t -> int *)
let rec tree_depth tree = match tree with
    Empty -> 0
  | Leaf(n) -> 0
  | Node(t1, n, t2) ->
     let d1 = (tree_depth t1) in
     let d2 = (tree_depth t2) in
     if d1 > d2 then
       d1 + 1
     else
       d2 + 1
;;


(* テスト *)
let test1 = tree_depth tree1 = 0 ;;
let test2 = tree_depth tree2 = 0 ;;
let test3 = tree_depth tree3 = 1 ;;
let test4 = tree_depth tree4 = 2 ;;

