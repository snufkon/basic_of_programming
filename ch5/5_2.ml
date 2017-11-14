(* 目的: 受け取った時間 x から、午後or午前を表す文字列を計算する *)
(* jikan : float -> string *)
let jikan x =
  if x < 12.0 then "am" else "pm"
;;


(* テスト *)
let test1 = jikan 1.0 = "am" ;;
let test2 = jikan 11.9 = "am" ;;
let test3 = jikan 12.0 = "pm" ;;
let test4 = jikan 18.5 = "pm" ;;
let test5 = jikan 23.9 = "pm" ;;
let test6 = jikan 24.0 = "pm" ;;
