(* 前提: ekimei_t ロード済み *)

(* 12.4 *)
(* 目的: 受け取った ekimei_t 型のリスト lst から駅の重複を取り除いたリストを返す *)
(* seiretsu: ekimei_t list -> ekimei_t list *)
let rec seiretsu lst =
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
  in
  (* 目的: 受け取った ekimei_t 型のリスト lst を kana フィールドの値で
     昇順に整列したリストを返す *)
  (* ekimei_sort: ekimei_t list -> ekimei_t list *)
  let rec ekimei_sort lst = match lst with
      [] -> []
    | first :: rest -> insert (ekimei_sort rest) first
  in
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
