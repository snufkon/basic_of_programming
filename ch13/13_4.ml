
(* 目的: 受け取った値 x を２倍して返す *)
(* times2: int -> int *)
let times2 x = x * 2 ;;

(* 目的: 受け取った値 x に３を足して返す *)
(* add3: int -> int *)
let add3 x = x + 3 ;;

(* 目的: 関数を２つ f1, f2 受け取りその２つの関数を合成した関数を返す *)
(* compose: ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let compose f1 f2 =
  let g x = f1 (f2 x) in
  g
;;

(* テスト *)
let test1 = (compose times2 add3) 4 = 14 ;;
let test2 = (compose times2 add3) 7 = 20 ;;
let test3 = (compose add3 times2) 4 = 11 ;;
let test4 = (compose add3 times2) 7 = 17 ;;

