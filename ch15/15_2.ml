(* 目的: ２つの自然数 m, n の最大公約数を求める

         最大公約数を求めるアルゴリズムとして
         ユークリッドの互除法を利用する

         入力は m >= n >= 0 とする
 *)
(* gcd: int -> int -> int *)
let rec gcd m n =
  if n = 0
  then m
  else gcd n (m mod n)
;;


(* テスト *)
let test1 = gcd 3 0 = 3 ;;
let test2 = gcd 5 0 = 5 ;;
let test3 = gcd 6 2 = 2 ;;
let test4 = gcd 6 3 = 3 ;;
let test4 = gcd 21 6 = 3 ;;


