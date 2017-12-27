#use "../metro/ekimei_t.ml" ;;
#use "../metro/eki_t.ml" ;;
#use "../metro/make_initial_eki_list.ml" ;;

(* テストデータ *)
let ekimei1 = {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"} ;;
let ekimei2 = {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"} ;;
let ekimei3 = {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"} ;;
let eki1 = {namae="代々木上原"; saitan_kyori=infinity; temae_list=[]} ;;
let eki2 = {namae="代々木公園"; saitan_kyori=infinity; temae_list=[]} ;;
let eki3 = {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[]} ;;


(* テスト *)
let test1 = make_initial_eki_list [] "代々木上原" = [] ;;
let test2 = make_initial_eki_list [ekimei1] "明治神宮前" = [eki1] ;;
let test3 = make_initial_eki_list [ekimei1; ekimei2; ekimei3] "代々木上原" =
              [{namae="代々木上原"; saitan_kyori=0.0; temae_list=["代々木上原"]}; eki2; eki3] ;;
