#use "../metro/ekimei_t.ml" ;;
#use "../metro/global_ekimei_list.ml" ;;
#use "../metro/romaji_to_kanji2.ml" ;;


(* テスト *)
let test1 = (try 
               romaji_to_kanji "test" global_ekimei_list
             with No_such_station(name) -> name) = "test" ;;
let test2 = romaji_to_kanji "akasaka" global_ekimei_list = "赤坂" ;;
let test3 = romaji_to_kanji "tokyo" global_ekimei_list = "東京" ;;
