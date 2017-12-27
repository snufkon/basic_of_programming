(* 前提: eki_t, ekikan_t, saitan_wo_bunri, koushin ロード済み *)

(* 16.4 *)
(* 目的: 未確定の駅のリスト eki_list, 駅間のリスト ekikan_list を
         受け取りダイクストラのアルゴリズムによって各駅について
         最短距離と最短経路が正しく入ったリストを返す
*)
(* dijkstra_main: eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main eki_list ekikan_list =
  if eki_list = [] then
    []
  else
    let (saitan, others) = saitan_wo_bunri eki_list in
    let eki_list2 = koushin saitan others ekikan_list in
    saitan :: dijkstra_main eki_list2 ekikan_list
;;
(* 未確定の駅リスト eki_list2 は１つずつ減っていくため停止する *)
