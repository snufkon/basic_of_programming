(* 人ひとり分のデータを表す型 *)
type person_t = {
  name : string;
  height : float;
  weight : float;
  birthday : string;
  blood_type : string;
} ;;

(* person_t list は
   - []             空リスト、あるいは
   - first :: rest  最初の要素が first で残りのリストが rest
                    (first は person_t 型、rest は自己参照のケース)
   という形 *)


(* person_t list 型のデータ例 *)
let lst1 = [] ;;
let lst2 = [{name = "suzuki"; height = 180.0; weight = 70.5; birthday = "1992"; blood_type = "O"}] ;;
let lst3 = [{name = "sato"; height = 172.5; weight = 66.3; birthday = "1990"; blood_type = "A"};
            {name = "tanaka"; height = 168.8; weight = 60.2; birthday = "2001"; blood_type = "B"}] ;;
let lst4 = [{name = "sato"; height = 172.5; weight = 66.3; birthday = "1990"; blood_type = "A"};
            {name = "tanaka"; height = 168.8; weight = 60.2; birthday = "2001"; blood_type = "B"};
            {name = "kato"; height = 175.8; weight = 63.3; birthday = "2002"; blood_type = "A"}] ;;


(* 目的: 受け取った person_t 型のデータリスト lst から血液型が A 型の人の数を返す *)
(* count_ketsueki_A: person_t list -> int *)
let rec count_ketsueki_A lst = match lst with
    [] -> 0
  | {name = n; height = h; weight = w; birthday = b; blood_type = bt} :: rest
    -> if bt = "A" then 1 + count_ketsueki_A rest
    else count_ketsueki_A rest
;;


(* テスト *)
let test1 = count_ketsueki_A lst1 = 0 ;;
let test2 = count_ketsueki_A lst2 = 0 ;;
let test3 = count_ketsueki_A lst3 = 1 ;;
let test4 = count_ketsueki_A lst4 = 2 ;;
