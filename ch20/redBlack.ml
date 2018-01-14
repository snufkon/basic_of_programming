(* 赤か黒を示す型 *)
type color_t = Red | Black ;;

(* 赤黒木を表す型 *)
type ('a, 'b) t = Empty
                | Node of ('a, 'b) t * 'a * 'b * color_t * ('a, 'b) t
;;

(* 空の木 *)
let empty = Empty 


(* 目的: t 型の赤黒木を受け取り、以下を行う関数
  子と孫の頂点が両方とも赤になっているかを調べる

  両方とも赤の場合は、木の再構成を行い赤の連続を解消した木を返す
  それ以外は、受け取った木をそのまま返す
*)
(* balance: t -> t *)
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


(* 目的: t 型の赤黒木、キー、値を受け取り、値を挿入した赤黒木を返す
   既に同じキーが存在する場合は新しく挿入する値で置き換える *)
(* insert: ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
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


(* 目的: t 型の赤黒木とキーを受け取り、キーに対応する値を返す *)
(* みつからなければ例外 Not_found を起こす *)
(* search: ('a, 'b) t -> 'a -> 'b *)
let rec search rb_tree k = match rb_tree with
    Empty -> raise Not_found
  | Node (left, key, value, color, right) ->
     if k = key then
       value
     else if k < key then
       search left k
     else search right k
;;
