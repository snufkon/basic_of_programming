#use "tree.ml" ;;


(* 木の例 *)
let tree1 = Empty ;;
let tree2 = Leaf (3) ;;
let tree3 = Node (tree1, 4, tree2) ;;
let tree4 = Node (tree2, 5, tree3) ;;

(* 目的: int -> int 型の関数 f と tree_t 型の木を受け取り
         節や葉に入っている値すべてに f を適用した木を返す
*)
(* tree_map: (int -> int) -> tree_t -> tree_t *)
let rec tree_map f tree = match tree with
    Empty -> tree
  | Leaf(n) -> Leaf(f n)
  | Node(t1, n, t2) -> Node((tree_map f t1), (f n), (tree_map f t2))
;;


(* テスト *)
let test1 = tree_map (fun x -> x+1) tree1 = tree1 ;;
let test2 = tree_map (fun x -> x+1) tree2 = Leaf (4) ;;
let test3 = tree_map (fun x -> x+1) tree3 = Node (Empty, 5, Leaf(4)) ;;
let test4 = tree_map (fun x -> x+1) tree4 = Node (Leaf(4), 6, Node(Empty, 5, Leaf(4))) ;;
