(* 人ひとり分のデータを表す型 *)
type person_t = {
  name : string;        (* 名前 *)
  height : float;       (* 身長(m) *)
  weight : float;       (* 体重(kg) *)
  birthday : string;    (* 誕生日(yyyymmdd) *)
  blood_type : string;  (* 血液型(A|B|O|AB) *)
} ;;


(* 目的: 人のデータ person を受け取り「◯◯さんの血液型は△型です」という文字列を返す *)
(* ketsueki_hyouji: person_t -> string *)
let ketsueki_hyouji person = match person with
    {name = n; height = h; weight = w; birthday = b; blood_type = bt; } ->
    n ^ "さんの血液型は" ^ bt ^ "型です"
;;


(* テスト *)
let test1 = ketsueki_hyouji {name="suzuki"; height=180.0; weight=70.5; birthday="1992"; blood_type="O"}
= "suzukiさんの血液型はO型です" ;;

let test2 = ketsueki_hyouji {name="sato"; height=172.5; weight=66.3; birthday="1990"; blood_type="A"}
= "satoさんの血液型はA型です" ;;

let test3 = ketsueki_hyouji {name="tanaka"; height=168.8; weight=60.2; birthday="2001"; blood_type="B"}
= "tanakaさんの血液型はB型です" ;;

let test4 = ketsueki_hyouji {name="yoshida"; height=175.8; weight=80.2; birthday="2000"; blood_type="AB"}
= "yoshidaさんの血液型はAB型です" ;;
