(* string list は
   - []             空リスト、あるいは
   - first :: rest  最初の要素が first で残りのリストが rest
                    (rest は自己参照のケース)
   という形 *)

(* 目的: 受け取った文字列のリスト lst に含まれる要素を
   前から順に全て結合した文字列を返す *)
(* concat string list -> string *)
let rec concat lst = match lst with
    [] -> ""
  | first :: rest -> first ^ concat rest
;;


(* テスト *)
let test1 = concat [] = "" ;;
let test2 = concat ["春"] = "春" ;;
let test3 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬" ;;
let test4 = concat ["冬"; "秋"; "夏"; "春"] = "冬秋夏春" ;;
let test5 = concat ["あ"; "い"; "うえ"; "お"] = "あいうえお" ;;
