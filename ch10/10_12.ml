#use "10_10.ml" ;;
#use "10_11.ml" ;;

(* 目的: ローマ字の駅名を２つ name1 name2 を受け取り駅間距離を調べ、以下の文字列を返す
   2駅が連結している場合: "A駅からB駅までは◯kmです"
   2駅が連結していない場合: "A駅とB駅はつながっていません"
   駅名が存在しない場合: "〜という駅は存在しません"
*)
(* kyori_wo_hyoji: string -> string * -> string *)
let kyori_wo_hyoji name1 name2 =
  let kanji_name1 = romaji_to_kanji name1 global_ekimei_list in
  let kanji_name2 = romaji_to_kanji name2 global_ekimei_list in
  if kanji_name1 = "" then
    name1 ^ "という駅は存在しません"
  else if kanji_name2 = "" then
    name2 ^ "という駅は存在しません"
  else
    let kyori = get_ekikan_kyori kanji_name1 kanji_name2 global_ekikan_list in
    if kyori = infinity then
      kanji_name1 ^ "駅と" ^ kanji_name2 ^ "駅はつながっていません"
    else
      kanji_name1 ^ "駅から" ^ kanji_name2 ^ "駅までは" ^ string_of_float kyori ^ "kmです"
;;


(* テスト *)
let test1 = kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅までは1.2kmです" ;;
let test2 = kyori_wo_hyoji "shinotsuka" "myogadani" = "新大塚駅から茗荷谷駅までは1.2kmです" ;;
let test3 = kyori_wo_hyoji "myogadani" "korakuen" = "茗荷谷駅から後楽園駅までは1.8kmです" ;;
let test4 = kyori_wo_hyoji "myogadani" "ikebukuro" = "茗荷谷駅と池袋駅はつながっていません" ;;
let test5 = kyori_wo_hyoji "myogadani" "hakata" = "hakataという駅は存在しません" ;;
let test6 = kyori_wo_hyoji "hakata" "myogadani" = "hakataという駅は存在しません" ;;
