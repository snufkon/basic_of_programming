(* 前提: ekimei_t.ml ロード済み *)

(* 10.10 *)
(* 目的: ローマ字の駅名 romaji と 駅名リスト lst を受け取り、
   その駅名の漢字表示を文字列で返す *)
(* romaji_to_kanji: string -> ekimei_t list -> string *)
let rec romaji_to_kanji name lst = match lst with
    [] -> ""
  | {kanji = kanji; kana = kana; romaji = romaji; shozoku = shozoku} :: rest ->
    if romaji = name
    then kanji
    else romaji_to_kanji name rest
;;
