(* 前提: ekikan_t, ekikan_tree_t, inserts_ekikan ロード済み *)
(* 17.13 *)
(* 目的: ekikan_tree_t 型の木と ekikan_t list 型の駅間リストを受け取り
   リストに含まれる駅間をすべて挿入した木を返す *)
(* inserts_ekikan: ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let rec inserts_ekikan tree ekikan_list =
  List.fold_left insert_ekikan tree ekikan_list
;;
