(* 学生ひとり分のデータ (名前, 点数, 成績) を表す型 *)
type gakusei_t = {
  namae : string;     (* 名前 *)
  tensuu : int;       (* 点数 *)
  seiseki : string;   (* 成績 *)
} ;;

let gakusei1 = {namae="suzuki"; tensuu=90; seiseki="A"} ;;
let gakusei2 = {namae="sato"; tensuu=65; seiseki="C"} ;;
let gakusei3 = {namae="tanaka"; tensuu=78; seiseki="B"} ;;

(* gakusei_t list は
   - []             空リスト、あるいは
   - first :: rest  最初の要素が first で残りのリストが rest
                    (first は gakusei_t 型, rest が自己参照のケース)
   という形 *)


(* 目的: tensuu フィールドの値で昇順に並んでいる gakusei_t 型のリスト lst と
   gakusei_t 型の値 gakusei を受け取り昇順を維持したまま gakusei を lst に
   追加したリストを返す *)
(* insert: gakusei_t list -> int -> gakusei_t list *)
let rec insert lst gakusei = match gakusei with
    {namae = n1; tensuu = t1; seiseki = s1} ->
    match lst with
      [] -> gakusei :: []
    | {namae = n2; tensuu = t2; seiseki = s2} as first :: rest ->
      if t1 <= t2 then
        gakusei :: (first :: rest)
      else
        first :: (insert rest gakusei)
;;


(* 目的: 受け取った gakusei_t 型のリスト lst を tensuu フィールドの値で
   昇順に整列したリストを返す *)
(* gakusei_sort: gakusei_t list -> gakusei_t list *)
let rec gakusei_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (gakusei_sort rest) first
;;


(* テスト *)
let test1 = insert [] gakusei1 = [gakusei1] ;;
let test2 = insert [gakusei1] gakusei2 = [gakusei2; gakusei1] ;;
let test3 = insert [gakusei2; gakusei1] gakusei3 = [gakusei2; gakusei3; gakusei1] ;;
let test4 = gakusei_sort [] = [] ;;
let test5 = gakusei_sort [gakusei1] = [gakusei1] ;;
let test6 = gakusei_sort [gakusei1; gakusei2; gakusei3] = [gakusei2; gakusei3; gakusei1] ;;
