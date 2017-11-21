(* 目的: 受け取った平面座標の組 point から x 軸について対象な点を返す *)
(* taisho_x: float * float -> float * float *)
let taisho_x point = match point with
    (x, y) -> (x, -. y)
;;


(* テスト *)
let test1 = taisho_x (0.0, 0.0) = (0.0, 0.0) ;;
let test2 = taisho_x (3.0, 2.0) = (3.0, -2.0) ;;
let test3 = taisho_x (-5.0, -3.0) = (-5.0, 3.0) ;;
