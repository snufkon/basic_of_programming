(* 目的: 与えられた数字 n のフィボナッチ数を返す *)
(* fib: int -> int *)
let rec fib n = 
  if n < 2 then n 
  else fib (n - 1) + fib (n - 2)
;;


(* 目的: 与えられた配列にフィボナッチ数を順に入れた配列を返す *)
(* fib_array: int array -> int array *)
let fib_array array =
  let n = Array.length array in
  let rec loop i =
    if i < n
    then (array.(i) <- fib i; loop (i + 1))
    else ()
  in
  (loop 0; array)
;;


(* テスト *)
let test1 = fib_array [|0; 0; 0; 0|] = [|0; 1; 1; 2|] ;;
let test2 = fib_array [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|] = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|] ;;
