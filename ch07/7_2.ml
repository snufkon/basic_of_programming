(* 目的: 受け取った名前と成績の組 pair から
   「... さんの評価は ... です」という文字列を計算する
*)
(* seiseki: string * string -> string *)
let seiseki pair = match pair with
    (name, result) -> name ^ "さんの評価は" ^ result ^ "です"
;;

(* テスト *)
let test1 = seiseki ("鈴木", "A") = "鈴木さんの評価はAです" ;;
let test2 = seiseki ("佐藤", "B") = "佐藤さんの評価はBです" ;;
let test3 = seiseki ("田中", "A") = "田中さんの評価はAです" ;;
