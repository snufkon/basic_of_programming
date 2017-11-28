(* メトロネットワーク最短経路問題における駅(頂点)を表すデータ型 *)
type eki_t = {
  namae: string;            (* 漢字の駅名 *)
  saitan_kyori: float;      (* 最短距離 *)
  temae_list: string list;  (* 駅名(漢字) のリスト *)
} ;;
