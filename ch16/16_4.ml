#use "../metro/get_ekikan_kyori.ml" ;;
#use "../metro/saitan_wo_bunri.ml" ;;

(* 16.3 *)
let koushin p v ekikan_list = match p with
    {namae=p_name; saitan_kyori=p_kyori; temae_list=p_team_list} -> 
    List.map (fun q -> match q with
                         {namae=q_name; saitan_kyori=q_kyori; temae_list=q_team_list}
                         -> 
                         let ekikan_kyori = get_ekikan_kyori p_name q_name ekikan_list in
                         if  ekikan_kyori = infinity then
                           q
                         else
                           let saitan_kyori = (p_kyori +. ekikan_kyori) in
                           if saitan_kyori < q_kyori then
                             {namae=q_name; saitan_kyori=saitan_kyori; temae_list= q_name :: p_team_list}
                           else
                             q)
      v
;;



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

(* テストデータ *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} ;;
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} ;;
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} ;;
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} ;;


(* テスト *)
let test1 = dijkstra_main [] global_ekikan_list = [] ;;
let test2 = dijkstra_main [eki1;eki2;eki3;eki4] global_ekikan_list = 
              [{namae = "茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]}; 
               {namae = "新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]}; 
               {namae = "後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]}; 
               {namae = "池袋"; saitan_kyori = 3.; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}] ;;
