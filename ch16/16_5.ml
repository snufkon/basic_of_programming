#use "../metro/eki_t.ml" ;;
#use "../metro/global_ekimei_list.ml" ;;
#use "../metro/global_ekikan_list.ml" ;;


(* 10.10 *)
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


(* 14.12 *)
(* 目的: ekimei_t 型のリスト ekimei_list と起点 name を受け取り、
   ekimei_t を eki_t 型のリストに変換する.

   また、起点のみ下記の変更を行う.

   - saitan_kyori に 0.0 を設定
   - temae_list は起点の駅名のみのリスト
*)
(* make_initial_eki_list: ekimei_t list -> string -> eki_t list *)
let make_initial_eki_list ekimei_list name =
  List.map (fun ekimei -> match ekimei with
                            {kanji=kanji; kana=kana; romaji=romaji; shozoku = shozoku} ->
                            if kanji = name then
                              {namae=name; saitan_kyori=0.0; temae_list=[name]}
                            else
                              {namae=kanji; saitan_kyori=infinity; temae_list=[]})
    ekimei_list
;;


(* 10.11 *)
(* 目的: 漢字の駅名２つ name1, name2 と 駅間リスト lst を受け取り、
   駅間リストの中からその２駅間の距離を返す *)
(* get_ekikan_kyori: string -> string -> ekikan_t list * -> float *)
let rec get_ekikan_kyori name1 name2 lst = match lst with
    [] -> infinity
  | {kiten=kiten; shuten=shuten; keiyu=keiyu; kyori=kyori; jikan=jikan} :: rest
    -> if name1 = kiten && name2 = shuten then kyori
    else if name1 = shuten && name2 = kiten then kyori
    else get_ekikan_kyori name1 name2 rest
;;


(* 15.5 *)
(* 目的: eki_t 型 のリストを受け取り、

   - 「最短距離最小の駅」
   - 「最短距離最小の駅以外からなるリスト」

   の組を返す *)
(* saitan_wo_bunri: eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri eki_list =
  let merge_result first rest_result = match rest_result with
      (p, v) -> 
      match (first, p) with
        ({namae=a_namae; saitan_kyori=a_kyori; temae_list=a_team_list},
         {namae=b_namae; saitan_kyori=b_kyori; temae_list=b_team_list})
        ->
        if b_namae = "" then
          (first, v)
        else if a_kyori < b_kyori then
          (first, p :: v)
        else
          (p, first :: v)
  in
  List.fold_right merge_result eki_list ({namae=""; saitan_kyori = infinity; temae_list = []}, [])
;;


(* 16.3 *)
let koushin p v ekikan_list = match p with
    {namae=p_name; saitan_kyori=p_kyori; temae_list=p_team_list} -> 
    List.map (fun q -> match q with
                         {namae=q_name; saitan_kyori=q_kyori; temae_list=q_team_list}
                         -> 
                         let ekikan_kyori = get_ekikan_kyori p_name q_name ekikan_list in
                         if  ekikan_kyori = infinity then
                           q
                         else
                           let saitan_kyori = (p_kyori +. ekikan_kyori) in
                           if saitan_kyori < q_kyori then
                             {namae=q_name; saitan_kyori=saitan_kyori; temae_list= q_name :: p_team_list}
                           else
                             q)
      v
;;


(* 目的: 未確定の駅のリスト eki_list, 駅間のリスト ekikan_list を
         受け取りダイクストラのアルゴリズムによって各駅について
         最短距離と最短経路が正しく入ったリストを返す
*)
(* dijkstra_main: eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main eki_list ekikan_list =
  if eki_list = [] then
    []
  else
    let (saitan, others) = saitan_wo_bunri eki_list in
    let eki_list2 = koushin saitan others ekikan_list in
    saitan :: dijkstra_main eki_list2 ekikan_list
;;


(* 目的: 駅リストから指定された名前の駅のレコードを返す *)
(* find_eki: eki_t list -> string -> eki_t *)
let find_eki eki_list namae =
  let result = List.filter (fun {namae=n; saitan_kyori=sk; temae_list=tm} -> n = namae)
                 eki_list
  in
  match result with
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
  | first :: rest -> first
;;


(* 目的: 始点の駅名(ローマ字の文字列)と終点の駅名(ローマ字の文字列)を受け取り、
   終点の駅のレコード(eki_t 型)を返す
 *)
(* dijkstra: string -> string -> eki_t *)
let dijkstra s_romaji e_romaji = 
  let normalized_ekimei_list = seiretsu global_ekimei_list in
  let s_kanji = romaji_to_kanji s_romaji normalized_ekimei_list in
  let e_kanji = romaji_to_kanji e_romaji normalized_ekimei_list in
  let eki_list = make_initial_eki_list normalized_ekimei_list s_kanji in
  let result_eki_list = dijkstra_main eki_list global_ekikan_list in
  find_eki result_eki_list e_kanji
;;


(* テスト *)
let test1 = dijkstra "shibuya" "gokokuji" = 
  {namae = "護国寺"; saitan_kyori = 9.8; 
   temae_list = 
     ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; 
      "青山一丁目"; "表参道"; "渋谷"]} ;;

let test2 = dijkstra "myogadani" "meguro" = 
  {namae = "目黒"; saitan_kyori = 12.7000000000000028; 
   temae_list = 
     ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王"; 
      "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]} ;;
