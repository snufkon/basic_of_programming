#use "../ch08/8_5.ml" ;;
#use "12_1.ml" ;;

(* 目的: 受け取った ekimei_t 型のリスト lst に含まれる駅名から
   eki_t 型のリストを作成する *)
(* make_eki_list: ekimei_t list -> eki_t list *)
let rec make_eki_list lst = match lst with
    [] -> []
  | {kanji=kanji; kana=kana; romaji=romaji; shozoku = shozoku} :: rest ->
    {namae=kanji; saitan_kyori=infinity; temae_list=[]} :: make_eki_list rest
;;


(* テストデータ *)
let ekimei1 = {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"} ;;
let ekimei2 = {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"} ;;
let ekimei3 = {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"} ;;


(* テスト *)
let test1 = make_eki_list [] = [] ;;
let test2 = make_eki_list [ekimei1] = [{namae="代々木上原"; saitan_kyori=infinity; temae_list=[]}] ;;
let test3 = make_eki_list [ekimei1; ekimei2; ekimei3] = [
    {namae="代々木上原"; saitan_kyori=infinity; temae_list=[]};
    {namae="代々木公園"; saitan_kyori=infinity; temae_list=[]};
    {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[]}]
;;
