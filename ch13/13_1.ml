#use "../ch08/8_3.ml" ;;

(* 目的: person_t 型のリスト lst を受け取り、その中から指定された
   血液型 blood_type の人の数を返す *)
(* count_ketsueki: person_t list -> string -> int *)
let rec count_ketsueki lst blood_type = match lst with
    [] -> 0
  | {name=n; height=h; weight=w; birthday=b; blood_type=bt} :: rest ->
    if bt = blood_type then
      1 + count_ketsueki rest blood_type
    else
      count_ketsueki rest blood_type
;;


(* テスト *)
let test1 = count_ketsueki [] "A" = 0 ;;
let test2 = count_ketsueki [person1; person2] "A" = 1 ;;
let test3 = count_ketsueki [person1; person2; person3; person4; person5] "A" = 2 ;;
let test4 = count_ketsueki [person1; person2; person3; person4; person5] "B" = 1 ;;
