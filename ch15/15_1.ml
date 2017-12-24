(* 目的: 受け取った lst をクイックソートを使って昇順に整列する *)
(* quick_sort: int list -> int list *)
let rec quick_sort lst =
  
  (* 目的: lst の中から n より p である要素のみを取り出す *)
  (* take: int -> int list -> (int -> int -> bool) -> int list *)
  let take n lst p = List.filter (fun item -> p item n) lst in
  
  (* 目的: lst の中から n より小さい要素のみを取り出す *)
  (* take_less: int -> int list -> int list *)
  let take_less n lst = take n lst (<=) in
  
  (* 目的 lst の中から n より大きい要素のみを取り出す *)
  (* take_greater: int -> int list -> int list *)
  let take_greater n lst = take n lst (>) in
  
  match lst with
    [] -> []
  | first :: rest -> quick_sort (take_less first rest)
                     @ [first]
                     @ quick_sort (take_greater first rest)
;;


(* テスト *)
let test1 = quick_sort [] = [] ;;
let test2 = quick_sort [1] = [1] ;;
let test3 = quick_sort [1; 2] = [1; 2] ;;
let test4 = quick_sort [2; 1] = [1; 2] ;;
let test5 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9] ;;

(* 元の quick_sort では以下のテストのような重複する要素を持つリストを整列することができない *)
let test6 = quick_sort [1; 1] = [1; 1] ;;
let test7 = quick_sort [5; 4; 9; 8; 4; 2] = [2; 4; 4; 5; 8; 9] ;;
