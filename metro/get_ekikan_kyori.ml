(* 前提: ekikan_t.ml ロード済み *)

(*10.11 *)
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
