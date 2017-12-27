#use "../metro/eki_t.ml" ;;
#use "../metro/ekikan_t.ml" ;;
#use "../metro/global_ekikan_list.ml" ;;
#use "../metro/get_ekikan_kyori.ml" ;;
#use "../metro/koushin.ml" ;;


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
