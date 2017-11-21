(* 目的: 鶴の数 x と 亀の数 y に応じた足の本数を計算する *)
(* int -> int -> int *)
let tsurukame_no_ashi x y = tsuru_no_ashi x + kame_no_ashi y ;;


(* テスト *)
let test1 = tsurukame_no_ashi 2 3 = 16 ;;
let test2 = tsurukame_no_ashi 3 2 = 14 ;;
let test3 = tsurukame_no_ashi 1 1 = 6 ;;
