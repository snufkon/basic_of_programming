(* 前提: eki_t, ekikan_t, saitan_wo_bunri, koushin2 ロード済み *)

(* 17.15 *)
(* 16.4 を ekian_tree_t を使い高速化 *)
(* 目的: 未確定の駅のリスト eki_list, 使用する駅間を持つ ekikan_tree_t 型の木
         を受け取りダイクストラのアルゴリズムによって各駅について
         最短距離と最短経路が正しく入ったリストを返す
*)
(* dijkstra_main: eki_t list -> ekikan_tree_t -> eki_t list *)
let rec dijkstra_main eki_list ekikan_tree =
  if eki_list = [] then
    []
  else
    let (saitan, others) = saitan_wo_bunri eki_list in
    let eki_list2 = koushin saitan others ekikan_tree in
    saitan :: dijkstra_main eki_list2 ekikan_tree
;;
