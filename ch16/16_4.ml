#use "../metro/eki_t.ml" ;;
#use "../metro/ekikan_t.ml" ;;
#use "../metro/get_ekikan_kyori.ml" ;;
#use "../metro/koushin.ml" ;;
#use "../metro/saitan_wo_bunri.ml" ;;
#use "../metro/global_ekikan_list.ml" ;;
#use "../metro/dijkstra_main.ml" ;;

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
