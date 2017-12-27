(* 前提: eki_t, ekimei_t ロード済み *)

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
