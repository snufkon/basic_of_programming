#use "../metro/ekikan_t.ml" ;;
#use "../metro/ekikan_tree_t.ml" ;;
#use "../metro/assoc.ml" ;;
#use "../metro/insert_ekikan.ml" ;;
#use "../metro/inserts_ekikan.ml" ;;
#use "../metro/get_ekikan_kyori2.ml" ;;
#use "../metro/global_ekikan_list.ml" ;;


(* テスト *)
let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list ;;
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2 ;;
let test2 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_tree = 1.2 ;;
let test3 = get_ekikan_kyori "茗荷谷" "後楽園" global_ekikan_tree = 1.8 ;;
let test4 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree = infinity ;;
let test5 = get_ekikan_kyori "東京" "銀座" global_ekikan_tree = 1.1 ;;
