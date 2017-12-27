#use "../metro/eki_t.ml" ;;
#use "../metro/get_ekikan_kyori.ml";;


(* 目的: 直前に確定した駅 p と未確定の駅のリスト v、使用する駅間のリスト ekikan_list
   を受け取り更新処理を行った後に未確定の駅リストを返す *)
(* koushin: eki_t -> eki_t list -> ekikan_list -> eki_t list *)
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


(* テストデータ *)
(* 確定した駅: eki2, eki3 *)
(* 未確定の駅: eki1, eki4 *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} ;;
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} ;;
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} ;;
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} ;;


(* テスト *)
let test1 = koushin eki2 [] global_ekikan_list = [] ;;
let test2 = koushin eki3 [eki1; eki4] global_ekikan_list =
              [eki1; {namae="後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]}] ;;
let test3 = koushin eki2 [eki1; eki4] global_ekikan_list =
              [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; eki4] ;; 
