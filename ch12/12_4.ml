#use "../ch08/8_5.ml"

(* 目的: kana フィールドの値で昇順に並んでいる ekimei_t 型のリスト lst と
   ekimei_t 型の値 ekimei を受け取り昇順を維持したまま ekimei を lst に
   追加したリストを返す *)
(* insert: ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec insert lst ekimei = match ekimei with
    {kanji=kanji1; kana=kana1; romaji=romaji1; shozoku=shozoku1} ->
    match lst with
      [] -> ekimei :: []
    | {kanji=kanji2; kana=kana2; romaji=romaji2; shozoku=shozoku2} as first :: rest ->
      if kana1 <= kana2 then
        ekimei :: (first :: rest)
      else
        first :: (insert rest ekimei)
;;


(* 目的: 受け取った ekimei_t 型のリスト lst を kana フィールドの値で
   昇順に整列したリストを返す *)
(* ekimei_sort: ekimei_t list -> ekimei_t list *)
let rec ekimei_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (ekimei_sort rest) first
;;


(* 目的: 受け取った ekimei_t 型のリスト lst から駅の重複を取り除いたリストを返す *)
(* seiretsu: ekimei_t list -> ekimei_t list *)
let rec seiretsu lst =
  let sorted_lst = ekimei_sort lst in
  match sorted_lst with
    [] -> []
  | {kanji=kanji1; kana=kana1; romaji=romaji1; shozoku=shozoku1} as first1 :: rest1 ->
    match rest1 with
      [] -> [first1]
    | {kanji=kanji2; kana=kana2; romaji=romaji2; shozoku=shozoku2} :: rest2 ->
      if kana1 = kana2 then
        seiretsu (first1 :: rest2)
      else
        first1 :: (seiretsu rest1)
;;


(* テストデータ *)
let ekimei1 = {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"} ;;
let ekimei2 = {kanji="綾瀬"; kana="あやせ"; romaji="ayase"; shozoku="千代田線"} ;;
let ekimei3 = {kanji="北綾瀬"; kana="きたあやせ"; romaji="kitaayase"; shozoku="千代田線"} ;;
let ekimei4 = {kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="南北線"} ;;
let ekimei5 = {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"} ;;
let ekimei6 = {kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="丸ノ内線"} ;;
let ekimei7 = {kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="丸ノ内線"} ;;

(* テスト *)
let test1 = insert [] ekimei1 = [ekimei1] ;;
let test2 = insert [ekimei1] ekimei2 = [ekimei2; ekimei1] ;;
let test3 = insert [ekimei2; ekimei1] ekimei3 = [ekimei2; ekimei3; ekimei1] ;;

let test4 = ekimei_sort [] = [] ;;
let test5 = ekimei_sort [ekimei1] = [ekimei1] ;;
let test6 = ekimei_sort [ekimei1; ekimei2; ekimei5] = [ekimei2; ekimei1; ekimei5] ;;
let test7 = ekimei_sort [ekimei1; ekimei2; ekimei3; ekimei4; ekimei5; ekimei6; ekimei7]
            = [ekimei6; ekimei2; ekimei3; ekimei1; ekimei5; ekimei4; ekimei7] ;;

let test1 = seiretsu [] = [] ;;
let test2 = seiretsu [ekimei1] = [ekimei1] ;;
let test3 = seiretsu [ekimei1; ekimei2; ekimei3; ekimei4; ekimei5; ekimei6; ekimei7]
            = [ekimei6; ekimei2; ekimei3; ekimei1; ekimei4] ;;
