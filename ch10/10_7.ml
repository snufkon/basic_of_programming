#use "../ch08/8_3.ml" ;;

(* person_t list は
   - []             空リスト、あるいは
   - first :: rest  最初の要素が first で残りのリストが rest
                    (first は person_t 型、 rest は自己参照のケース)
   という形 *)


(* 目的: 受け取った person_t 型のリスト lst から各血液型の人が何人いるかを組にして返す *)
(* ketsueki_shukei: person_t list -> int * int * int * int *)
let rec ketsueki_shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {name = n; height = h; weight = w; birthday = bd; blood_type = bt} :: rest ->
    let (a, b, o, ab) = ketsueki_shukei rest in
    if bt = "A" then (a + 1, b, o, ab)
    else if bt = "B" then (a, b + 1, o, ab)
    else if bt = "O" then (a, b, o + 1, ab)
    else (a, b, o, ab + 1)
;;


(* テスト *)
let test1 = ketsueki_shukei [] = (0, 0, 0, 0) ;;
let test2 = ketsueki_shukei [person1] = (0, 0, 1, 0) ;;
let test3 = ketsueki_shukei [person1; person2; person3; person4; person5] = (2, 1, 1, 1) ;;
