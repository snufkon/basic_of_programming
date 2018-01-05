#use "../metro/eki_t.ml" ;;
#use "../metro/ekikan_t.ml" ;;
#use "../metro/ekimei_t.ml" ;;
#use "../metro/ekikan_tree_t.ml" ;;
#use "../metro/global_ekimei_list.ml" ;;
#use "../metro/global_ekikan_list.ml" ;;
#use "../metro/seiretsu.ml" ;;
#use "../metro/romaji_to_kanji.ml" ;;
#use "../metro/assoc.ml" ;;
#use "../metro/insert_ekikan.ml" ;;
#use "../metro/inserts_ekikan.ml" ;;
#use "../metro/get_ekikan_kyori2.ml" ;;
#use "../metro/koushin2.ml" ;;
#use "../metro/saitan_wo_bunri.ml" ;;
#use "../metro/dijkstra_main2.ml" ;;
#use "../metro/make_initial_eki_list.ml" ;;

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


(* 16.5 を ekian_tree_t を使い高速化 *)
(* 目的: 始点の駅名(ローマ字の文字列)と終点の駅名(ローマ字の文字列)を受け取り、
   終点の駅のレコード(eki_t 型)を返す
 *)
(* dijkstra: string -> string -> eki_t *)
let dijkstra s_romaji e_romaji =
  let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list in
  let normalized_ekimei_list = seiretsu global_ekimei_list in
  let s_kanji = romaji_to_kanji s_romaji normalized_ekimei_list in
  let e_kanji = romaji_to_kanji e_romaji normalized_ekimei_list in
  let eki_list = make_initial_eki_list normalized_ekimei_list s_kanji in
  let result_eki_list = dijkstra_main eki_list global_ekikan_tree in
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
