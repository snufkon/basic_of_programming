#use "../ch09/9_10.ml" ;;
#use "../metro/get_ekikan_kyori.ml";;


(* テスト *)
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2 ;;
let test2 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2 ;;
let test3 = get_ekikan_kyori "茗荷谷" "後楽園" global_ekikan_list = 1.8 ;;
let test4 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_list = infinity ;;
let test5 = get_ekikan_kyori "東京" "銀座" global_ekikan_list = 1.1 ;;
