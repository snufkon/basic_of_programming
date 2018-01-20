#use "../metro/eki_t.ml" ;;
#use "../metro/print_eki.ml" ;;

(* テストデータ *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} ;;
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} ;;
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} ;;
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} ;;

(* 結果確認 *)
print_eki eki1 ;;
print_eki eki2 ;;
print_eki eki3 ;;
print_eki eki4 ;;
