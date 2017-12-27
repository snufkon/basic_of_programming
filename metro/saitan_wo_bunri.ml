(* 前提: eki_t.ml ロード済み *)

(* 15.4 *)
(* 目的: eki_t 型 のリストを受け取り、

   - 「最短距離最小の駅」
   - 「最短距離最小の駅以外からなるリスト」

   の組を返す *)
(* saitan_wo_bunri: eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri eki_list =
  let merge_result first rest_result = match rest_result with
      (p, v) -> 
      match (first, p) with
        ({namae=a_namae; saitan_kyori=a_kyori; temae_list=a_team_list},
         {namae=b_namae; saitan_kyori=b_kyori; temae_list=b_team_list})
        ->
        if b_namae = "" then
          (first, v)
        else if a_kyori < b_kyori then
          (first, p :: v)
        else
          (p, first :: v)
  in
  List.fold_right merge_result eki_list ({namae=""; saitan_kyori = infinity; temae_list = []}, [])
;;
