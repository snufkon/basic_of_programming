#use "../ch08/8_3.ml" ;;

(* 目的: person_t 型の値 person を受け取り、その人の名前を返す *)
(* one_person_namae: person_t -> string *)
let one_person_namae person = match person with
    {name=n; height=h; weight=w; birthday=b; blood_type=bt} -> n
;;

(* 目的: person_t 型のリスト lst を受け取り、その中に出てくる人の名前のリストを返す *)
(* person_namae: person_t list -> string list *)
let person_namae lst =
  List.map one_person_namae lst
;;

(* テスト *)
let test1 = one_person_namae person1 = "suzuki" ;;
let test2 = one_person_namae person2 = "sato" ;;
let test3 = person_namae [] = [] ;;
let test4 = person_namae [person1] = ["suzuki"] ;;
let test5 = person_namae [person1; person2] = ["suzuki"; "sato"] ;;
let test6 = person_namae [person1; person2; person3; person4; person5] =
            ["suzuki"; "sato"; "tanaka"; "yoshida"; "kato"] ;;
