(* 前提: ekikan_tree_t ロード済み *)

(* 18.4 *)
(* 目的: 漢字の駅名２つ name1, name2 と ekikan_tree_t 型の木を受け取り、
   その２駅間の距離を返す *)
(* みつからないときには Not_found の例外が発生する *)
(* get_ekikan_kyori: string -> string -> ekikan_tree_t -> float *)
let get_ekikan_kyori name1 name2 tree =
  (* 目的: 漢字の駅名と ekikan_tree_t 型の木を受け取り、
     駅名の Node があればその Node、なければ Empty を返す *)
  (* get_node: string -> ekikan_tree_t -> ekikan_tree_t *)
  let rec get_node ekimei tree = match tree with
      Empty -> Empty
    | Node(t1,n_ekimei,n_lst,t2) ->
       if ekimei = n_ekimei then
         Node(t1,n_ekimei,n_lst,t2)
       else if ekimei <  n_ekimei then
         get_node ekimei t1
       else
         get_node ekimei t2
  in
  match (get_node name1 tree) with
    Empty -> raise Not_found
  | Node(t1,n_ekimei,n_lst,t2) ->
     List.assoc name2 n_lst
;;
