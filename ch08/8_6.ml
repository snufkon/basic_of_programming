(* 駅ひとつ分のデータを表す型 *)
type ekimei_t = {
  kanji : string;   (* 漢字の駅名 *)
  kana : string;    (* ひらがなの駅名 *)
  romaji : string;  (* ローマ字の駅名 *)
  shozoku: string;  (* 駅が所属する路線名 *)
} ;;


(* 目的: 駅のデータ ekimei を受け取り、「路線名、駅名（かな）」の文字列を返す *)
(* hyouji: ekimei_t -> string *)
let hyouji ekimei = match ekimei with
    {kanji = kanji; kana = kana; romaji = romaji; shozoku = shozoku;}
    -> shozoku ^ "、" ^ kanji ^ "（" ^ kana ^ "）"
;;


(* テスト *)
let test1 = hyouji {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"}
            = "丸ノ内線、茗荷谷（みょうがだに）" ;;

let test2 = hyouji {kanji="新宿"; kana="しんじゅく"; romaji="shinjuku"; shozoku="山手線"}
            = "山手線、新宿（しんじゅく）" ;;

let test3 = hyouji {kanji="立川"; kana="たちかわ"; romaji="tachikawa"; shozoku="中央線"}
            = "中央線、立川（たちかわ）" ;;
