#use "20_2.ml" ;;

(* 目的: rb_tree_t 型の赤黒木、キー、値を受け取り、値を挿入した赤黒木を返す
   既に同じキーが存在する場合は新しく挿入する値で置き換える *)
(* insert: ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t *)
let insert rb_tree key value = 
  let rec insert1 rb_tree = match rb_tree with
      Empty -> Node (Empty, key, value, Red, Empty)
    | Node(lt, k, v, c, rt) ->
       if key < k then
         balance(Node(insert1 lt, k, v, c, rt))
       else if key > k then
         balance(Node(lt, k, v, c, insert1 rt))
       else
         Node(lt, k, value, c, rt)
         
  in match insert1 rb_tree with
       Empty -> assert false
     | Node(lt, k, v, c, rt) -> Node(lt, k, v, Black, rt)
;;


(* テスト *)
let rb_tree0 = Empty ;;
let rb_tree1 = insert Empty 10 "x" ;;
let rb_tree2 = insert rb_tree1 13 "y" ;;
let rb_tree3 = insert rb_tree2 15 "z" ;;
let rb_tree4 = insert rb_tree3 15 "zz" ;; 

let test1 = rb_tree1 = Node (Empty, 10, "x", Black, Empty) ;;
let test2 = rb_tree2 = Node (Empty,
                             10, "x", Black,
                             Node(Empty, 13, "y", Red, Empty)) ;;
let test3 = rb_tree3 = Node (Node (Empty, 10, "x", Black, Empty), 
			     13, "y", Black, 
			     Node (Empty, 15, "z", Black, Empty)) ;;
let test4 = rb_tree4 = Node (Node (Empty, 10, "x", Black, Empty), 
			     13, "y", Black, 
			     Node (Empty, 15, "zz", Black, Empty)) ;;
