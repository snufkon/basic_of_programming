(* 目的: 受け取った５教科の点数から、その合計点と平均点をする *)
(* goukei_to_heikin: int -> int -> int -> int -> int -> int * float *)
let goukei_to_heikin kokugo suugaku eigo rika shakai =
  (kokugo + suugaku + eigo + rika + shakai,
   float_of_int (kokugo + suugaku + eigo + rika + shakai) /. 5.0)
;;


(* テスト *)
let test1 = goukei_to_heikin 0 0 0 0 0 = (0, 0.0) ;;
let test2 = goukei_to_heikin 10 20 30 40 50 = (150, 30.0) ;;
let test3 = goukei_to_heikin 80 70 50 90 60 = (350, 70.0) ;;
