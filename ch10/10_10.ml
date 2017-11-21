#use "../ch09/9_9.ml" ;;

(* 目的: ローマ字の駅名 romaji と 駅名リスト lst を受け取り、
   その駅名の漢字表示を文字列で返す *)
(* romaji_to_kanji: string -> ekimen_t list -> string *)
let rec romaji_to_kanji name lst = match lst with
    [] -> ""
  | {kanji = kanji; kana = kana; romaji = romaji; shozoku = shozoku} :: rest ->
    if romaji = name
    then kanji
    else romaji_to_kanji name rest
;;


(* テスト *)
let test1 = romaji_to_kanji "test" global_ekimei_list = "" ;;
let test2 = romaji_to_kanji "akasaka" global_ekimei_list = "赤坂" ;;
let test3 = romaji_to_kanji "tokyo" global_ekimei_list = "東京" ;;
