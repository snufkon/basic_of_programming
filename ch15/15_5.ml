#use "../metro/eki_t.ml" ;;

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


(* テストデータ *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} ;;
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} ;;
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} ;;
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} ;;


(* テスト *)
let test1 = saitan_wo_bunri [eki1; eki2; eki3; eki4] = (eki3, [eki1; eki2; eki4]) ;;
