#use "../metro/ekimei_t.ml" ;;
#use "../metro/eki_t.ml" ;;

(* 目的: 受け取った ekimei_t 型のリスト lst に含まれる駅名から
   eki_t 型のリストを作成する *)
(* make_eki_list: ekimei_t list -> eki_t list *)
let make_eki_list lst = 
  List.map (fun ekimei -> match ekimei with
                            {kanji=kanji; kana=kana; romaji=romaji; shozoku = shozoku} ->
                            {namae=kanji; saitan_kyori=infinity; temae_list=[]}) lst
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


(* 目的: eki_t 型のリスト lst と起点 name を受け取り、
   起点のみ下記の変更を行ったリストを返す

   - saitan_kyori に 0.0 を設定
   - temae_list は起点の駅名のみのリスト
*)
(* shokika: eki_t list -> string -> eki_t list *)
let shokika lst name =
  List.map (fun eki -> match eki with
                         {namae=namae; saitan_kyori=kyori; temae_list=temae_list}->
                         if name = namae then
                           {namae=name; saitan_kyori=0.0; temae_list=[name]}
                         else
                           eki)
    lst
;;


(* テストデータ *)
let eki1 = {namae="代々木上原"; saitan_kyori=infinity; temae_list=[]} ;;
let eki2 = {namae="代々木公園"; saitan_kyori=infinity; temae_list=[]} ;;
let eki3 = {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[]} ;;

(* テスト *)
let test1 = shokika [] "代々木上原" = [] ;;
let test2 = shokika [eki1; eki2; eki3] "代々木上原" = [
    {namae="代々木上原"; saitan_kyori=0.0; temae_list=["代々木上原"]};
    eki2;
    eki3
  ] ;;
let test3 = shokika [eki1; eki2; eki3] "明治神宮前" = [
    eki1;
    eki2;
    {namae="明治神宮前"; saitan_kyori=0.0; temae_list=["明治神宮前"]}
  ] ;;
