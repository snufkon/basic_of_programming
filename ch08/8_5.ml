(* 駅ひとつ分のデータを表す型 *)
type ekimei_t = {
  kanji : string;   (* 漢字の駅名 *)
  kana : string;    (* ひらがなの駅名 *)
  romaji : string;  (* ローマ字の駅名 *)
  shozoku: string;  (* 駅が所属する路線名 *)
} ;;
