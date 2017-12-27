#use "../ch09/9_9.ml" ;;
#use "../metro/romaji_to_kanji.ml" ;;

(* テスト *)
let test1 = romaji_to_kanji "test" global_ekimei_list = "" ;;
let test2 = romaji_to_kanji "akasaka" global_ekimei_list = "赤坂" ;;
let test3 = romaji_to_kanji "tokyo" global_ekimei_list = "東京" ;;
