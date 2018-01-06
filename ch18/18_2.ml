(* 目的: item の値段を調べる *)
(* price: string -> (string * int) list -> int option *)
let rec price item yaoya_list = match yaoya_list with
    [] -> None
  | (yasai, nedan) :: rest ->
     if item = yasai then
       Some (nedan)
     else
       price item rest
;;


(* 目的: 野菜のリストと八百屋のリストを受け取り、野菜のリストのうち
         八百屋にはおいていない野菜の数を返す *)
(* count_urikire_yasai: string list -> (string * int) list -> int *)
let rec count_urikire_yasai yasai_list yaoya_list = match yasai_list with
    [] -> 0
  | first :: rest ->
     match price first yaoya_list with
       None -> 1 + count_urikire_yasai rest yaoya_list
     | Some(p) -> count_urikire_yasai rest yaoya_list
;;


(* 八百屋においてある野菜と値段のリストの例 *)
let yaoya_list = [("トマト", 300); ("たまねぎ", 200); ("にんじん", 150); ("ほうれん草", 200)] ;;


(* テスト *)
let test1 = count_urikire_yasai ["トマト"; "にんじん"] yaoya_list = 0 ;;
let test1 = count_urikire_yasai ["トマト"; "にんじん"; "きゃべつ"] yaoya_list = 1 ;;
let test1 = count_urikire_yasai ["トマト"; "にんじん"; "きゃべつ"; "にんじん"] yaoya_list = 1 ;;

