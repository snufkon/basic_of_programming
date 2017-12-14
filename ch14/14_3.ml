(* 目的: init から初めて lst の要素を右から順に f を流し込む *)
(* folod_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f lst init = match lst with
    [] -> init
  | first :: rest -> f first (fold_right f rest init)
;;


(* 目的: 与えられた２つの文字列 s1 s2 を結合する *)
(* join_string: string -> string -> string *)
let join_string s1 s2 =
  s1 ^ s2
;;


(* 目的: 受け取った文字列のリスト lst に含まれる要素を
   前から順に全て結合した文字列を返す *)
(* concat string list -> string *)
let concat lst =
  fold_right join_string lst ""
;; 


(* テスト *)
let test1 = concat [] = "" ;;
let test2 = concat ["春"] = "春" ;;
let test3 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬" ;;
let test4 = concat ["冬"; "秋"; "夏"; "春"] = "冬秋夏春" ;;
let test5 = concat ["あ"; "い"; "うえ"; "お"] = "あいうえお" ;;
