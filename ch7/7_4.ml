(* 目的: 受け取ったふたつの平面座標の組 point1 point2 からその中点を返す *)
(* chuten: float * float -> float * float -> float * float *)
let chuten point1 point2 =
  match point1 with
    (x1, y1) ->
    match point2 with
      (x2, y2) -> ((x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0)
;;


(* テスト *)
let test1 = chuten (0.0, 0.0) (0.0, 0.0) = (0.0, 0.0) ;;
let test2 = chuten (3.0, 2.0) (5.0, -3.0) = (4.0, -0.5) ;;
let test3 = chuten (-5.0, 3.0) (5.0, 8.0) = (0.0, 5.5) ;;
