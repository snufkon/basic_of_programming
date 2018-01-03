(* 年号を表す型 *)
type nengou_t = Meiji of int    (* 明治 *)
              | Taisho of int   (* 対象 *)
              | Showa of int    (* 昭和 *)
              | Heisei of int   (* 平成 *)
;;

(* 目的: 年号を受け取ったら対応する西暦年を返す *)
(* to_seireki: nengou_t -> int *)
let to_seireki nengou = match nengou with
    Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1988
;;

                        
(* 目的: 誕生年と現在の年を nengou_t 型の値として受け取り、年齢を返す *)
(* nenrei: nengou_t -> nengou_t -> int *)
let nenrei birth_nengou current_nengou =
  let birth_seireki = to_seireki birth_nengou in
  let current_seireki = to_seireki current_nengou in
  current_seireki - birth_seireki
;;


(* テスト *)
let test1 = nenrei (Showa(52)) (Heisei(29)) = 40 ;;
let test2 = nenrei (Heisei(20)) (Heisei(29)) = 9 ;;
