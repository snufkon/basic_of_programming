(* 17.11 *)
(* 目的: 「駅名」と「駅名と距離の組のリスト」を受け取り、その駅の距離を返す *)
(* assoc: string -> (string * float) list -> float *)
let rec assoc name lst = match lst with
    [] -> infinity
  | (name2, kyori) :: rest ->
     if name = name2 then
       kyori
     else
       assoc name rest
;;
