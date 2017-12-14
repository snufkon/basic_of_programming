
(* 目的: 整数を受け取り x その2乗から1を引いた数を返す *)
fun x -> x * x + 1 ;;

(* テスト *)
let test1 = (fun x -> x * x + 1) 0 = 1 ;;
let test2 = (fun x -> x * x + 1) 2 = 5 ;;
let test3 = (fun x -> x * x + 1) 3 = 10 ;;

