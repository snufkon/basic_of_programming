(* 見つからなかったことを示す例外 *)
exception Not_found ;;


(* 目的: 「駅名」と「駅名と距離の組のリスト」を受け取り、その駅の距離を返す *)
(* 見つからないときには Not_found という例外が発生する *)
(* assoc: 'a -> ('a * 'b) list -> 'b *)
let rec assoc name lst = match lst with
    [] -> raise Not_found
  | (name2, kyori) :: rest ->
     if name = name2 then
       kyori
     else
       assoc name rest
;;


(* テスト *)
let test1 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8 ;;
let test2 = (try
               assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)]
             with Not_found -> infinity) = infinity ;;
