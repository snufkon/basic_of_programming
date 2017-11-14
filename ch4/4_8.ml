(* 目的: 鶴と亀の数の合計 x と 足の数の合計 y に応じた鶴の数を計算する *)
(* int -> int -> int *)
let tsurukame x y = (x * 4 - y) / 2 ;;


(* テスト *)
let test1 = tsurukame 2 4 = 2 ;;
let test2 = tsurukame 3 8 = 2 ;;
let test3 = tsurukame 4 16 = 0 ;;
