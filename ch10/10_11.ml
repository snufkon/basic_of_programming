#use "../ch09/9_10.ml" ;;

(* 目的: 漢字の駅名２つ name1, name2 と 駅間リスト lst を受け取り、
   駅間リストの中からその２駅間の距離を返す *)
(* get_ekikan_kyori: string -> string -> ekikan_t list * -> float *)
let rec get_ekikan_kyori name1 name2 lst = match lst with
    [] -> infinity
  | {kiten=kiten; shuten=shuten; keiyu=keiyu; kyori=kyori; jikan=jikan} :: rest
    -> if name1 = kiten && name2 = shuten then kyori
    else if name1 = shuten && name2 = kiten then kyori
    else get_ekikan_kyori name1 name2 rest
;;


(* テスト *)
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2 ;;
let test2 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2 ;;
let test3 = get_ekikan_kyori "茗荷谷" "後楽園" global_ekikan_list = 1.8 ;;
let test4 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_list = infinity ;;
let test5 = get_ekikan_kyori "東京" "銀座" global_ekikan_list = 1.1 ;;
