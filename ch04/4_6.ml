
(* 目的: 鶴の数 x に応じた足の本数を計算する *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi x = x * 2 ;;

(* 目的: 亀の数 x に応じた足の本数を計算する *)
(* kame_no_ashi : int -> int *)
let kame_no_ashi x = x * 4 ;;

(* テスト *)
let test1 = tsuru_no_ashi 1 = 2 ;;
let test2 = tsuru_no_ashi 5 = 10 ;;
let test3 = tsuru_no_ashi 0 = 0 ;;

let test4 = kame_no_ashi 1 = 4 ;;
let test5 = kame_no_ashi 5 = 20 ;;
let test6 = kame_no_ashi 0 = 0 ;;
