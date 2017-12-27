#use "../metro/eki_t.ml" ;;
#use "../metro/ekikan_t.ml" ;;
#use "../metro/global_ekikan_list.ml" ;;
#use "../metro/get_ekikan_kyori.ml";;


(* 目的: 直前に確定した駅 p と未確定の駅 q を受け取り、
   p と q が直接つながっている場合に q の最短距離と手前リストの更新を行う *)
(* koushin1: eki_t -> eki_t -> eki_t *)
let koushin1 p q = match (p, q) with
    {namae=p_name; saitan_kyori=p_kyori; temae_list=p_team_list},
    {namae=q_name; saitan_kyori=q_kyori; temae_list=q_team_list}
    -> 
    let ekikan_kyori = get_ekikan_kyori p_name q_name global_ekikan_list in
    if  ekikan_kyori = infinity then
      q
    else
      let saitan_kyori = (p_kyori +. ekikan_kyori) in
      if saitan_kyori < q_kyori then
        {namae=q_name; saitan_kyori=saitan_kyori; temae_list= q_name :: p_team_list}
      else
        q
;;


(* テストデータ *)
(* 確定した駅: eki2, eki3 *)
(* 未確定の駅: eki1, eki4 *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} ;;
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} ;;
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} ;;
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} ;;

(* テスト *)
let test1 = koushin1 eki3 eki1 = eki1 ;;
let test2 = koushin1 eki3 eki2 = eki2 ;;
let test3 = koushin1 eki3 eki3 = eki3 ;;
let test4 = koushin1 eki3 eki4 = {namae="後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]} ;;
let test5 = koushin1 eki2 eki1 = {namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]} ;;
let test6 = koushin1 eki2 eki2 = eki2 ;;
let test7 = koushin1 eki2 eki3 = eki3 ;;
let test8 = koushin1 eki2 eki4 = eki4 ;;
