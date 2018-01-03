(* 前提: ekikan_t, ekikan_tree_t ロード済み *)

(* 17.12 *)
(* 目的: ekikan_tree_t 型の木と ekikan_t 型 の駅間を受け取り、その情報を挿入した木を返す *)
(* insert_ekikan: ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let insert_ekikan tree ekikan =
  (* 目的: ekikan_tree_t 型の木と駅名、「その駅に直接つながっている駅名」と「その駅までの距離」の組
     を受け取り、その情報を挿入した木を返す
   *)
  (* insert: ekikan_tree_t -> string -> (string * float) -> ekikan_tree_t *)
  let rec insert tree ekimei ekimei_to_kyori = match tree with
      Empty -> Node(Empty, ekimei, [ekimei_to_kyori], Empty)
    | Node(t1,n_ekimei,n_lst,t2) ->
       if ekimei < n_ekimei then
         Node((insert t1 ekimei ekimei_to_kyori), n_ekimei, n_lst, t2)
       else if ekimei > n_ekimei then
         Node(t1, n_ekimei, n_lst, (insert t2 ekimei ekimei_to_kyori))
       else
         Node(t1, n_ekimei, ekimei_to_kyori :: n_lst, t2)
  in
  match ekikan with
    {kiten=kiten; shuten=shuten; keiyu=keiyu; kyori=kyori; jikan=jikan} ->
    insert (insert tree shuten (kiten, kyori)) kiten (shuten, kyori)
;;
