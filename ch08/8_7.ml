(* 駅間ひとつ分のデータを表す型 *)
type ekikan_t = {
  kiten : string;   (* 起点の駅名 *)
  shuten : string;  (* 終点の駅名 *)
  keiyu : string;   (* 経由する路線名 *)
  kyori : float;    (* ２駅間の距離(km) *)
  jikan : int;      (* ２駅間の所要時間(分) *)
} ;;
