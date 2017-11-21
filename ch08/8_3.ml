(* 人ひとり分のデータを表す型 *)
type person_t = {
  name : string;
  height : float;
  weight : float;
  birthday : string;
  blood_type : string;
} ;;

let person1 = {name = "suzuki"; height = 180.0; weight = 70.5; birthday = "1992"; blood_type = "O"} ;;
let person2 = {name = "sato"; height = 172.5; weight = 66.3; birthday = "1990"; blood_type = "A"} ;;
let person3 = {name = "tanaka"; height = 168.8; weight = 60.2; birthday = "2001"; blood_type = "B"} ;;
let person4 = {name = "yoshida"; height = 158.2; weight = 50.2; birthday = "2003"; blood_type = "A"} ;;
let person5 = {name = "kato"; height = 188.2; weight = 80.8; birthday = "1988"; blood_type = "AB"} ;;
